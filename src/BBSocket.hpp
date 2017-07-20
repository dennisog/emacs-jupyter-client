//
// BBsocket.cc
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e1992b21d361fe148544408730cca197a1207c3b
#define e1992b21d361fe148544408730cca197a1207c3b

#include "Message.hpp"

#include <cstdint>
#include <string>
#include <vector>
#include <zmq.hpp>

namespace ejc {
using msg::raw_message;
//
// A convenience wrapper around the 0MQ socket
//
class BBSocket {
public:
  // housekeeping
  BBSocket(zmq::context_t &ctx, int flags);
  void connect(std::string const &endpoint);
  void bind(std::string const &endpoint);
  // polling
  bool pollin(long timeout);
  bool pollout(long timeout);
  // blocking i/o
  bool send(raw_message &data);
  void send_multipart(std::vector<raw_message> &data);
  std::vector<raw_message> recv_multipart();

private:
  bool poll_(int flags, long timeout);
  zmq::socket_t sock_;
  zmq::pollitem_t pi_;
};

} // namespace ejc

#endif // e1992b21d361fe148544408730cca197a1207c3b
