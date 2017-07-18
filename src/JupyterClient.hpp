//
// JupyterClient.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e067987744dae1b61d9d4099702a42c702346337
#define e067987744dae1b61d9d4099702a42c702346337

#include <json/json.h>
#include <zmq.hpp>

#include <exception>
#include <string>
#include <vector>

namespace ejc {

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

private:
  //
  // The connection to the kernel
  //
  std::string connection_file_name_;
  zmq::context_t ctx_;

  // interpret a connection file and save some methods
  void parse_connection_file_();
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
