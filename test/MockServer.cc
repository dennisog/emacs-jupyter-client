//
// A mock server for debugging the emacs pupyter client
//

#include "BBSocket.hpp"
#include "JupyterClient.hpp"
#include <boost/format.hpp>
#include <cassert>
#include <chrono>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <json/json.h>
#include <sstream>
#include <string>

inline std::string get_endpoint(std::string const &transport,
                                boost::asio::ip::address_v4 ip,
                                unsigned int port) {
  return (boost::format("%s://%s:%d") % transport % ip.to_string() % port)
      .str();
}

std::string now() {
  auto now =
      std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
  std::stringstream s;
  s << std::put_time(std::localtime(&now), "[%Y-%m-%d %X]: ");
  return s.str();
}

int main(int argc, const char *argv[]) {

  assert(argc == 2);
  std::vector<uint8_t> outdelim(100, '0');

  // set up the sockets
  zmq::context_t ctx(1);
  ejc::BBSocket control(ctx, ZMQ_ROUTER);
  ejc::BBSocket shell(ctx, ZMQ_ROUTER);
  ejc::BBSocket stdin(ctx, ZMQ_ROUTER);
  ejc::BBSocket hb(ctx, ZMQ_ROUTER);
  ejc::BBSocket iopub(ctx, ZMQ_PUB);

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

  // loop and print
  std::cout << "in main loop" << std::endl;
  while (true) {
    // check the heartbeart socket
    if (hb.pollin(10)) {
      auto rx = hb.recv_multipart();
      std::string str;
      str.assign(begin(rx[1]), end(rx[1]));
      std::cout << now() << "Received heartbeat: " << str << std::endl;
      // respond to the ping
      std::vector<ejc::msg::raw_message> out;
      out.push_back(rx[0]);
      out.push_back({'p', 'o', 'n', 'g'});
      hb.send_multipart(out);
    }
    // check the shell socket
    if (shell.pollin(10)) {
      auto rx = shell.recv_multipart();
      std::cout << now() << "Received message on shell buffer:\n";
      std::string delim(rx[1].data(), rx[1].size());
      std::cout << "Delim: " << delim << "\n";
      std::string hmac(rx[2].data(), rx[2].size());
      std::cout << "HMAC: " << hmac << "\n";
      std::string header(rx[3].data(), rx[3].size());
      std::cout << "header: " << header << "\n";
      std::string parent_header(rx[4].data(), rx[4].size());
      std::cout << "parent_header: " << parent_header << "\n";
      std::string metadata(rx[5].data(), rx[5].size());
      std::cout << "meatadata: " << metadata << "\n";
      std::string content(rx[6].data(), rx[6].size());
      std::cout << "content: " << content << "\n";
      // dump to file
      // std::ofstream of("mock_server_data", std::ios::app | std::ios::binary);
      // for (auto const &msg : rx) {
      //   of.write((char *)msg.data(), msg.size());
      // }
      // of.write((char *)outdelim.data(), outdelim.size());
      // of.close();
    }
  }
}
