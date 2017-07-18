//
// A mock server for debugging the emacs pupyter client
//

#include "JupyterClient.hpp"
#include <boost/format.hpp>
#include <cassert>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <json/json.h>
#include <string>
#include <zmq.hpp>

inline std::string get_endpoint(std::string const &transport,
                                boost::asio::ip::address_v4 ip,
                                unsigned int port) {
  return (boost::format("%s://%s:%d") % transport % ip.to_string() % port)
      .str();
}

int main(int argc, const char *argv[]) {

  assert(argc == 2);

  // set up the sockets
  zmq::context_t ctx(1);
  zmq::socket_t control(ctx, ZMQ_ROUTER);
  zmq::socket_t shell(ctx, ZMQ_ROUTER);
  zmq::socket_t stdin(ctx, ZMQ_ROUTER);
  zmq::socket_t hb(ctx, ZMQ_ROUTER);
  zmq::socket_t iopub(ctx, ZMQ_PUB);

  // get info about the connection
  std::cout << "parsing connection file" << std::endl;
  std::string connection_file_name(argv[1]);
  auto cp = ejc::KernelManager::parse_connection_file_(connection_file_name);

  // connect the sockets
  std::cout << "connecting" << std::endl;
  control.bind(get_endpoint(cp.transport, cp.ip, cp.control_port));
  shell.bind(get_endpoint(cp.transport, cp.ip, cp.shell_port));
  stdin.bind(get_endpoint(cp.transport, cp.ip, cp.stdin_port));
  hb.bind(get_endpoint(cp.transport, cp.ip, cp.hb_port));
  iopub.bind(get_endpoint(cp.transport, cp.ip, cp.iopub_port));
  std::vector<zmq::pollitem_t> pi = {
      {static_cast<void *>(control), 0, ZMQ_POLLIN | ZMQ_POLLOUT, 0},
      {static_cast<void *>(shell), 0, ZMQ_POLLIN | ZMQ_POLLOUT, 0},
      {static_cast<void *>(stdin), 0, ZMQ_POLLIN | ZMQ_POLLOUT, 0},
      {static_cast<void *>(hb), 0, ZMQ_POLLIN | ZMQ_POLLOUT, 0},
      {static_cast<void *>(iopub), 0, ZMQ_POLLIN | ZMQ_POLLOUT, 0}};

  // loop and print
  std::cout << "in main loop" << std::endl;
  while (true) {
    // poll until we have something
    zmq::poll(pi.data(), pi.size(), -1);
    // check the heartbeart socket
    if ((pi[3].revents & ZMQ_POLLIN) == ZMQ_POLLIN) {
      zmq::message_t msg;
      zmq::message_t id;
      hb.recv(&id);  // first message is the id
      hb.recv(&msg); // then comes the message
      std::string str;
      str.assign(static_cast<char *>(msg.data()), msg.size());
      auto now = std::chrono::system_clock::to_time_t(
          std::chrono::system_clock::now());
      std::cout << std::put_time(std::localtime(&now), "[%Y-%m-%d %X]: ")
                << "Received heartbeat: " << str << std::endl;
      // respond to the ping
      std::string pong("pong");
      zmq::message_t out(pong.length());
      memcpy(out.data(), pong.data(), pong.length());
      hb.send(id, ZMQ_SNDMORE);
      hb.send(out);
    }
  }
}
