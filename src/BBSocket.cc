//
// BBsocket.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "BBSocket.hpp"

namespace ejc {

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

} // namespace ejc
