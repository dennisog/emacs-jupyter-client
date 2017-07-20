//
// JupyterClient.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "JupyterClient.hpp"

#include <fstream>
#include <iostream>
#include <random>

namespace ejc {

//
// A convenience wrapper around the 0MQ socket
//
BBSocket::BBSocket(zmq::context_t &ctx, int flags)
    : sock_(ctx, flags), pi_{.socket = static_cast<void *>(sock_),
                             .fd = 0,
                             .events = ZMQ_POLLIN | ZMQ_POLLOUT,
                             .revents = 0} {}
void BBSocket::connect(std::string const &endpoint) { sock_.connect(endpoint); }
void BBSocket::bind(std::string const &endpoint) { sock_.bind(endpoint); }

// polling
bool BBSocket::pollin(long timeout) { return poll_(ZMQ_POLLIN, timeout); }
bool BBSocket::pollout(long timeout) { return poll_(ZMQ_POLLOUT, timeout); }
bool BBSocket::poll_(int flags, long timeout) {
  pi_.events = flags;
  auto ret = zmq::poll(&pi_, 1, timeout);
  if (ret != 0) {
    return (pi_.revents & flags) == flags ? true : false;
  } else {
    return false;
  }
}

// blocking i/o
bool BBSocket::send(raw_message &data) {
  zmq::message_t msg(data.data(), data.size(), NULL);
  return sock_.send(msg);
}
void BBSocket::send_multipart(std::vector<raw_message> &data) {
  // definitely not the most efficient way of doing this...
  zmq::message_t msg;
  for (size_t i = 0; i < data.size(); ++i) {
    auto &buf = data[i];
    msg.rebuild(buf.data(), buf.size(), NULL);
    if (i == data.size() - 1) {
      sock_.send(msg, 0);
    } else {
      sock_.send(msg, ZMQ_SNDMORE);
    }
  }
}
// receive a full multipart message
std::vector<raw_message> BBSocket::recv_multipart() {
  std::vector<raw_message> out;
  zmq::message_t msg;
  bool more = true;
  while (more) {
    sock_.recv(&msg);
    raw_message tmp;
    auto msg_data = static_cast<char *>(msg.data());
    tmp.insert(end(tmp), msg_data, msg_data + msg.size());
    out.push_back(tmp);
    more = [this] {
      int64_t sz_inout = 0;
      size_t len = sizeof(sz_inout);
      sock_.getsockopt(ZMQ_RCVMORE, &sz_inout, &len);
      return sz_inout == 0 ? false : true;
    }();
  }
  return out;
}

//
// A communication channel to the Jupyter kernel
//
Channel::Channel(zmq::context_t &ctx, int flags,
                 std::function<void(std::vector<raw_message>)> rx_handler)
    : rx_handler_(rx_handler), sock_(ctx, flags), running_(false) {}

void Channel::connect(std::string const &endpoint) { sock_.connect(endpoint); }

bool Channel::send(raw_message &data) {
  std::lock_guard<std::mutex> lock(sockmtx_);
  return sock_.send(data);
}

void Channel::send_multipart(std::vector<raw_message> &data) {
  std::lock_guard<std::mutex> lock(sockmtx_);
  return sock_.send_multipart(data);
}

bool Channel::running() {
  std::lock_guard<std::mutex> lock(loopmtx_);
  return running_;
}

void Channel::start() {
  if (!running()) {
    running_ = true;
    rx_thread_ = std::thread([this] { run(); });
  }
}

void Channel::run() {
  while (true) {
    if (!running()) {
      break;
    } else {
      std::lock_guard<std::mutex> lock(sockmtx_);
      // FIXME hardcoded polling timeout
      if (sock_.pollin(10)) {
        auto msgs = sock_.recv_multipart();
        rx_handler_(msgs);
      }
    }
  }
}

// a heartbeat channel is only slightly different from a regular channel
HBChannel::HBChannel(zmq::context_t &ctx, int flags,
                     std::chrono::milliseconds timeout,
                     std::chrono::milliseconds interval,
                     std::function<void(bool)> notify_manager)
    : Channel(ctx, flags, [](auto a) {}), timeout_(timeout),
      interval_(interval), notify_manager_(notify_manager) {}

// send a 'ping' to the kernel, wait for a response and timeout if no response.
void HBChannel::run_heartbeat_() {
  using namespace std::chrono;
  std::mt19937 mt(system_clock::now().time_since_epoch().count());
  std::uniform_real_distribution<> r(0, 1);
  auto rand = [&mt, &r] { return r(mt); };

  while (true) {
    if (!running()) {
      break;
    } else {
      auto start = system_clock::now();
      sock_.send(ping_);
      {
        std::lock_guard<std::mutex> lock(sockmtx_);
        if (sock_.pollin(timeout_.count())) {
          auto buf = sock_.recv_multipart();
          notify_manager_(true);
        } else {
          notify_manager_(false);
        }
      }
      auto elapsed = duration_cast<milliseconds>(system_clock::now() - start);
      auto sleepfor = rand() * (interval_ - elapsed);
      std::this_thread::sleep_for(sleepfor);
    }
  }
}

void HBChannel::start() {
  if (!running()) {
    running_ = true;
    rx_thread_ = std::thread([this] { run_heartbeat_(); });
  }
}

// debug dummy handler
void print_message_received(std::vector<raw_message> msgs) {
  std::cerr << "Message received." << std::endl;
}

KernelManager::KernelManager(std::string const &connection_file,
                             unsigned int nthreads)
    : connection_file_name_(connection_file), ctx_(zmq::context_t(nthreads)),
      cparams_(parse_connection_file_(connection_file)),
      control_chan_(ctx_, ZMQ_DEALER, print_message_received),
      shell_chan_(ctx_, ZMQ_DEALER, print_message_received),
      stdin_chan_(ctx_, ZMQ_DEALER, print_message_received),
      // FIXME timeout and interval values are hardcoded rn
      hb_chan_(ctx_, ZMQ_DEALER, std::chrono::milliseconds(100),
               std::chrono::milliseconds(2500),
               [this](auto a) {
                 std::lock_guard<std::mutex> lock(hbmtx_);
                 alive_ = a;
               }),
      iopub_chan_(ctx_, ZMQ_PUB, print_message_received), alive_(false) {}

const ConnectionParams
KernelManager::parse_connection_file_(std::string const &fn) {
  Json::Value dict;
  std::ifstream infile(fn);
  infile >> dict;
  infile.close();
  return ConnectionParams(
      {.ip = boost::asio::ip::address_v4::from_string(dict["ip"].asString()),
       .key = dict["key"].asString(),
       .transport = dict["transport"].asString(),
       .control_port = dict["control_port"].asUInt(),
       .shell_port = dict["shell_port"].asUInt(),
       .stdin_port = dict["stdin_port"].asUInt(),
       .hb_port = dict["hb_port"].asUInt(),
       .iopub_port = dict["iopub_port"].asUInt(),
       .signature_scheme = ([](auto in) {
         if (in.compare("hmac-sha256") == 0) {
           return ConnectionParams::SignatureScheme::HMAC_SHA256;
         } else {
           throw std::runtime_error(
               (boost::format("Unsupported Signature scheme: %s") % in).str());
         }
       })(dict["signature_scheme"].asString())});
}

// is the kernel alive?
bool KernelManager::is_alive() {
  std::lock_guard<std::mutex> lock(hbmtx_);
  return alive_;
}

void KernelManager::connect() {
  using boost::format;
  int i = 5; // number of tries FIXME hardcoded
  for (; i > 0; --i) {
    try {
      // connct the channels to their endpoints and start them
      control_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                             cparams_.ip.to_string() % cparams_.control_port)
                                .str());
      shell_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.shell_port)
                              .str());
      stdin_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.stdin_port)
                              .str());
      hb_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                        cparams_.ip.to_string() % cparams_.hb_port)
                           .str());
      iopub_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.iopub_port)
                              .str());
      control_chan_.start();
      shell_chan_.start();
      stdin_chan_.start();
      hb_chan_.start();
      iopub_chan_.start();
      break;
    } catch (std::exception &ex) {
      // FIXME
      ;
    }
  }
  if (i <= 0) {
    throw std::runtime_error("Connection failure.");
  }
}

JupyterClient::JupyterClient(KernelSpec const &kspec,
                             std::string const &connection_file)
    : sessionid_(msg::uuidgen()), kspec_(kspec), km_(connection_file),
      hmac_(km_.key()) {}

//
// This is a quick hack to get a finalizer in the emacs C interface
//
void JupyterClient::del(void *client) noexcept {
  delete static_cast<JupyterClient *>(client);
}

//
// Connection
//
void JupyterClient::connect() { km_.connect(); }
bool JupyterClient::alive() { return km_.is_alive(); }

//
// client stuff
//

// execute some code
const std::string JupyterClient::execute_code(std::string const &code,
                                              bool silent, bool store_history,
                                              bool allow_stdin,
                                              bool stop_on_error) {
  using std::make_unique;
  // header
  auto header =
      make_unique<msg::Header>("execute_request", "emacs-user", sessionid_);
  auto id = header->msg_id;
  // parent header
  msg::Header::uptr parent_header = nullptr;
  // metadata
  msg::Metadata::uptr metadata = nullptr;
  // content
  msg::Content::uptr content = make_unique<msg::ExecuteRequest>(
      code, silent, store_history, nullptr, allow_stdin, stop_on_error);
  // buffers
  msg::Buffers::uptr buffers = nullptr;
  // the message
  auto msg = std::make_unique<msg::Message>(
      std::move(header), std::move(parent_header), std::move(metadata),
      std::move(content), std::move(buffers));
  // serialize & send
  auto raw_msgs = serialize_(std::move(msg));
  km_.shell().send_multipart(raw_msgs);
  return id;
}

// serialize a message to a buffer of multipart message parts
std::vector<raw_message> JupyterClient::serialize_(msg::Message::uptr m) {
  std::vector<raw_message> to_send;
  // get the serialized bytes of the message
  if (m->header != nullptr)
    to_send.push_back(m->header->serialize());
  if (m->parent_header != nullptr)
    to_send.push_back(m->parent_header->serialize());
  if (m->metadata != nullptr)
    to_send.push_back(m->metadata->serialize());
  if (m->content != nullptr)
    to_send.push_back(m->content->serialize());
  // FIXME looks like we do not need to prepend the 'ident'?
  std::vector<raw_message> msgs;
  msgs.push_back(msg::msg_delim);
  msgs.push_back(sign_(to_send));
  for (auto &buf : to_send)
    msgs.push_back(std::move(buf));
  if (m->buffers != nullptr)
    for (auto &buf : m->buffers->data)
      msgs.push_back(std::move(buf));
  return msgs;
}

// get the signature of a block of messages
raw_message JupyterClient::sign_(std::vector<raw_message> data) {
  raw_message r{'c'};
  return r;
}

} // namespace ejc
