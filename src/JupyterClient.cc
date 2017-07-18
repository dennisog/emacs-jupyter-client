//
// JupyterClient.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "JupyterClient.hpp"

#include <fstream>
#include <iostream>

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

// polling
bool BBSocket::poll_(int flags, long timeout) {
  auto ret = zmq::poll(&pi_, 1, timeout);
  return ret == 0 ? false : true;
}
bool BBSocket::pollin(long timeout) { return poll_(ZMQ_POLLIN, timeout); }
bool BBSocket::pollout(long timeout) { return poll_(ZMQ_POLLOUT, timeout); }

// blocking i/o
bool BBSocket::send(raw_message data) {
  zmq::message_t msg(data.size());
  memcpy(msg.data(), data.data(), data.size());
  return sock_.send(msg);
}
// receive a full multipart message
raw_message BBSocket::recv_multipart() {
  raw_message out;
  zmq::message_t msg;
  bool more = true;
  while (more) {
    sock_.recv(&msg);
    auto msg_data = static_cast<char *>(msg.data());
    out.insert(end(out), msg_data, msg_data + msg.size());
    more = [this] {
      int sz_inout = 0;
      size_t len = 1;
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
                 std::function<void(raw_message)> rx_handler)
    : rx_handler_(rx_handler), sock_(ctx, flags), running_(false) {}

void Channel::connect(std::string const &endpoint) { sock_.connect(endpoint); }

bool Channel::send(raw_message data) {
  std::lock_guard<std::mutex> lock(sockmtx_);
  return sock_.send(data);
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
        auto buf = sock_.recv_multipart();
        rx_handler_(buf);
      }
    }
  }
}

// debug
void print_message_received(raw_message msgs) {
  std::cerr << "Message received." << std::endl;
}

KernelManager::KernelManager(std::string const &connection_file,
                             unsigned int nthreads)
    : connection_file_name_(connection_file), ctx_(zmq::context_t(nthreads)),
      cparams_(parse_connection_file_(connection_file)),
      control_chan_(ctx_, ZMQ_DEALER, print_message_received),
      shell_chan_(ctx_, ZMQ_DEALER, print_message_received),
      stdin_chan_(ctx_, ZMQ_DEALER, print_message_received),
      hb_chan_(ctx_, ZMQ_DEALER, print_message_received),
      iopub_chan_(ctx_, ZMQ_PUB, print_message_received) {}

const KernelManager::ConnectionParams
KernelManager::parse_connection_file_(std::string const &fn) {
  Json::Value dict;
  std::ifstream infile(fn);
  infile >> dict;
  infile.close();
  return KernelManager::ConnectionParams(
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

// send a heartbeat, wait for receipt
bool KernelManager::is_alive() { return true; }

void KernelManager::connect() {
  using boost::format;
  int i = 5; // number of tries FIXME hardcoded
  for (; i > 0; --i) {
    try {
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
      break;
    } catch (std::exception &ex) {
      ;
    }
  }
  if (i <= 0) {
    throw std::runtime_error("Connection failure.");
  }
  if (is_alive()) {
    return;
  }
  throw std::runtime_error("Connection failure.");
}

JupyterClient::JupyterClient(KernelSpec const &kspec,
                             std::string const &connection_file)
    : kspec_(kspec), km_(connection_file) {}

//
// This is a quick hack to get a finalizer in the emacs C interface
//
void JupyterClient::del(void *client) {
  delete static_cast<JupyterClient *>(client);
}

//
// Connection
//
void JupyterClient::connect() { km_.connect(); }

} // namespace ejc
