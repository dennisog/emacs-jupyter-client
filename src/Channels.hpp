//
// Channels.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef ad0357cb9bf3b211ce5ffececf25a50d385464b2
#define ad0357cb9bf3b211ce5ffececf25a50d385464b2

#include "BBSocket.hpp"
#include "Message.hpp"

#include <zmq.hpp>

#include <atomic>
#include <mutex>
#include <thread>

namespace ejc {

using msg::raw_message;

//
// A communication channel to the Jupyter kernel
//
class Channel {
public:
  Channel(zmq::context_t &ctx, int flags,
          std::function<void(std::vector<raw_message>)> rx_handler);
  void connect(std::string const &endpoint);
  bool send(raw_message &data);
  void send_multipart(std::vector<raw_message> &data);
  bool running();
  void start();
  void stop();

protected:
  // each channel needs a handler which process received messages
  std::function<void(std::vector<raw_message>)> rx_handler_;
  // communication
  BBSocket sock_;
  // threading
  std::atomic_bool running_;
  void run();
  std::mutex sockmtx_;
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
  raw_message ping_ = {'p', 'i', 'n', 'g'};
  std::function<void(bool)> notify_manager_;
};

// the iopub channel holds a SUB socket, so we need to subscribe to all topics
// before starting
class IOPubChannel : public Channel {
public:
  IOPubChannel(zmq::context_t &ctx, int flags,
               std::function<void(std::vector<raw_message>)> rx_handler)
      : Channel(ctx, flags, rx_handler) {
    sock_.get().setsockopt(ZMQ_SUBSCRIBE, "", 0);
  }
};

} // namespace ejc

#endif // ad0357cb9bf3b211ce5ffececf25a50d385464b2
