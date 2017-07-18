//
// JupyterClient.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e067987744dae1b61d9d4099702a42c702346337
#define e067987744dae1b61d9d4099702a42c702346337

#include <boost/asio.hpp>
#include <boost/format.hpp>
#include <json/json.h>
#include <zmq.hpp>

#include <exception>
#include <string>
#include <unordered_map>
#include <vector>

namespace ejc {

//
// 0MQ Polling
//
class Poller {
public:
  Poller(){};
  void add(zmq::socket_t &sock, short events);
  void remove(zmq::socket_t &sock); // costly
  int poll(long timeout);
  bool check_for(zmq::socket_t &sock, short event);
  bool check(zmq::socket_t &sock);

private:
  std::vector<zmq::pollitem_t> _pi;
  std::unordered_map<void *, std::pair<short, short>> _tbl;
};

// https://jupyter-client.readthedocs.io/en/latest/kernels.html#kernelspecs
struct KernelSpec {
  std::string argv_;
  std::string display_name_;
  std::string language_;
  KernelSpec(std::string const &argv, std::string const &display_name,
             std::string const &language)
      : argv_(argv), display_name_(display_name), language_(language) {}
};

class KernelManager {
public:
  KernelManager(std::string const &connection_file, unsigned int nthreads = 1);

  void connect();
  bool is_alive();

private:
  //
  // The connection to the kernel
  //
  std::string connection_file_name_;
  zmq::context_t ctx_;
  zmq::socket_t control_sock_;
  zmq::socket_t shell_sock_;
  zmq::socket_t stdin_sock_;
  zmq::socket_t hb_sock_;
  zmq::socket_t iopub_sock_;
  Poller poll_;

  // wire format parameters
  struct ConnectionParams {
    boost::asio::ip::address_v4 ip;
    std::string key;
    std::string transport;
    unsigned int control_port;
    unsigned int shell_port;
    unsigned int stdin_port;
    unsigned int hb_port;
    unsigned int iopub_port;
    enum class SignatureScheme { HMAC_SHA256 };
    SignatureScheme signature_scheme;
  };
  ConnectionParams cparams_;

  // interpret a connection file
  const ConnectionParams parse_connection_file_(std::string const &fn);
};

class JupyterClient {
public:
  JupyterClient(KernelSpec const &kspec, std::string const &connection_file);
  static void del(void *client);

  //
  // connection-related
  //
  void connect();

private:
  // Info about the kernel
  KernelSpec kspec_;
  // The connection to the kernel
  KernelManager km_;
};

} // namespace ejc

#endif // e067987744dae1b61d9d4099702a42c702346337
