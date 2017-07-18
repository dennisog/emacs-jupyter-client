//
// JupyterClient.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "JupyterClient.hpp"

#include <fstream>

namespace ejc {

//
// Simple 0MQ poller implementation
//

void Poller::add(zmq::socket_t &sock, short events) {
  if (_tbl.find(static_cast<void *>(sock)) != end(_tbl))
    return;
  _pi.push_back({.socket = static_cast<void *>(sock),
                 .fd = 0,
                 .events = events,
                 .revents = 0});
  _tbl.emplace(static_cast<void *>(sock),
               std::make_pair(_pi.size() - 1, events));
}

void Poller::remove(zmq::socket_t &sock) {
  auto s = _tbl.find(static_cast<void *>(sock));
  if (s == end(_tbl))
    return;
  _pi.erase(begin(_pi) + s->second.first);
  _tbl.clear();
  for (size_t i = 0; i < _pi.size(); ++i) {
    _tbl.emplace(_pi[i].socket, std::make_pair(i, _pi[i].events));
  }
}

int Poller::poll(long timeout) {
  return zmq::poll(_pi.data(), _pi.size(), timeout);
}

bool Poller::check_for(zmq::socket_t &sock, short event) {
  return _pi[_tbl[static_cast<void *>(sock)].first].revents == event ? true
                                                                     : false;
}

bool Poller::check(zmq::socket_t &sock) {
  auto p = _tbl[static_cast<void *>(sock)];
  return _pi[p.first].revents == p.second ? true : false;
}

KernelManager::KernelManager(std::string const &connection_file,
                             unsigned int nthreads)
    : connection_file_name_(connection_file), ctx_(zmq::context_t(nthreads)),
      cparams_(parse_connection_file_(connection_file)),
      control_sock_(ctx_, ZMQ_DEALER), shell_sock_(ctx_, ZMQ_DEALER),
      stdin_sock_(ctx_, ZMQ_DEALER), hb_sock_(ctx_, ZMQ_DEALER),
      iopub_sock_(ctx_, ZMQ_PUB) {}

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
bool KernelManager::is_alive() {
  std::string ping("ping");
  zmq::message_t out(ping.length());
  zmq::message_t in;
  memcpy(out.data(), ping.data(), ping.length());
  poll_.poll(5);
  int i = 5;
  for (; i > 0; --i) {
    if (poll_.check_for(control_sock_, ZMQ_POLLOUT))
      break;
    poll_.poll(10);
  }
  if (i < 0)
    return false;
  control_sock_.send(out);
  // wait for rx
  poll_.poll(5);
  i = 5;
  for (; i > 0; --i) {
    if (poll_.check_for(control_sock_, ZMQ_POLLIN))
      break;
    poll_.poll(10);
  }
  if (i < 0)
    return false;
  control_sock_.recv(&in);
  ;
  if (in.size() != ping.length()) {
    return false;
  } else {
    std::string p2;
    p2.assign(static_cast<char *>(in.data()), in.size());
    return p2.compare(ping);
  }
} // this is the worst I have written i am tired now

void KernelManager::connect() {
  using boost::format;
  int i = 5; // number of tries
  for (; i > 0; --i) {
    try {
      control_sock_.connect((format("%s://%s:%d") % cparams_.transport %
                             cparams_.ip.to_string() % cparams_.control_port)
                                .str());
      shell_sock_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.shell_port)
                              .str());
      stdin_sock_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.stdin_port)
                              .str());
      hb_sock_.connect((format("%s://%s:%d") % cparams_.transport %
                        cparams_.ip.to_string() % cparams_.hb_port)
                           .str());
      iopub_sock_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.iopub_port)
                              .str());
      poll_.add(control_sock_, ZMQ_POLLOUT | ZMQ_POLLIN);
      poll_.add(shell_sock_, ZMQ_POLLOUT | ZMQ_POLLIN);
      poll_.add(stdin_sock_, ZMQ_POLLOUT | ZMQ_POLLIN);
      poll_.add(hb_sock_, ZMQ_POLLOUT | ZMQ_POLLIN);
      poll_.add(iopub_sock_, ZMQ_POLLIN);
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
