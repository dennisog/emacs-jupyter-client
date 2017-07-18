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

#include <chrono>
#include <cstdint>
#include <exception>
#include <functional>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace ejc {

// there are better ways of abstracting buffers, but this works for now
typedef std::vector<uint8_t> raw_message;

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
  bool send(raw_message data);
  std::vector<raw_message> recv_multipart();

private:
  bool poll_(int flags, long timeout);
  zmq::socket_t sock_;
  zmq::pollitem_t pi_;
};

//
// A communication channel to the Jupyter kernel
//
class Channel {
public:
  Channel(zmq::context_t &ctx, int flags,
          std::function<void(std::vector<raw_message>)> rx_handler);
  void connect(std::string const &endpoint);
  bool send(raw_message data);
  bool running();
  void start();
  void stop();

protected:
  // each channel needs a handler which process received messages
  std::function<void(std::vector<raw_message>)> rx_handler_;
  // communication
  BBSocket sock_;
  // threading
  bool running_;
  void run();
  std::mutex sockmtx_, loopmtx_;
  std::thread rx_thread_;
};

// a heartbeat channel is only slightly different from a regular channel. It
// runs in a loop an randomly pings the kernel. If the kernel fails to respond
// withing timeout milliseconds, we call the notify_manager function with
// 'false' as argument.
class HBChannel : public Channel {
public:
  HBChannel(zmq::context_t &ctx, int flags, std::chrono::milliseconds timeout,
            std::chrono::milliseconds interval,
            std::function<void(bool)> notify_manager);
  void start();

private:
  void run_heartbeat_();
  std::chrono::milliseconds timeout_;
  std::chrono::milliseconds interval_;
  const raw_message ping_ = {'p', 'i', 'n', 'g'};
  std::function<void(bool)> notify_manager_;
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

class KernelManager {
public:
  KernelManager(std::string const &connection_file, unsigned int nthreads = 1);

  void connect();
  bool is_alive();
  // interpret a connection file
  static const ConnectionParams parse_connection_file_(std::string const &fn);

private:
  //
  // The connection to the kernel
  //
  std::string connection_file_name_;
  zmq::context_t ctx_;
  ConnectionParams cparams_;
  // channels
  Channel control_chan_;
  Channel shell_chan_;
  Channel stdin_chan_;
  HBChannel hb_chan_;
  Channel iopub_chan_;

  // heartbeat
  std::mutex hbmtx_;
  bool alive_;
};

class JupyterClient {
public:
  JupyterClient(KernelSpec const &kspec, std::string const &connection_file);
  static void del(void *client) noexcept;

  //
  // connection-related
  //
  void connect();
  bool alive();

private:
  // Info about the kernel
  KernelSpec kspec_;
  // The connection to the kernel
  KernelManager km_;
};

} // namespace ejc

#endif // e067987744dae1b61d9d4099702a42c702346337
