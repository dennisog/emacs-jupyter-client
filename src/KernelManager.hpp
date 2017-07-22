//
// KernelManager.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e686e5e5a21a52e79afd8eabc64d64ef458ff95c
#define e686e5e5a21a52e79afd8eabc64d64ef458ff95c

#include "BBQueue.hpp"
#include "BBSocket.hpp"
#include "Channels.hpp"

#include <boost/asio.hpp>
#include <zmq.hpp>

#include <atomic>
#include <string>

namespace ejc {

// types be getting too long...
typedef std::function<void(std::vector<msg::raw_message>)> handler_fn;
typedef bbq::BBQueue<Json::Value> Messagequeue;

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
  KernelManager(ConnectionParams &cparams, unsigned int nthreads,
                handler_fn control_handler, handler_fn shell_handler,
                handler_fn stdin_handler, handler_fn iopub_handler);
  ~KernelManager();
  void connect();
  bool alive() { return alive_ ? true : false; }

  Channel &control() { return control_chan_; }
  Channel &shell() { return shell_chan_; }
  Channel &stdin() { return stdin_chan_; }
  Channel &heartbeat() { return hb_chan_; }
  Channel &iopub() { return iopub_chan_; }

  // status
  enum class Status { Busy, Idle, Starting };
  bool busy() { return status_ == Status::Busy ? true : false; }
  bool idle() { return status_ == Status::Idle ? true : false; }
  void status(Status s) { status_ = s; }

private:
  //
  // The connection to the kernel
  //
  zmq::context_t ctx_;
  ConnectionParams &cparams_;
  // channels
  Channel control_chan_;
  Channel shell_chan_;
  Channel stdin_chan_;
  HBChannel hb_chan_;
  IOPubChannel iopub_chan_;

  // heartbeat / status
  std::atomic_bool alive_;
  std::atomic<Status> status_;

  // can't copy this
  KernelManager(KernelManager const &other) = delete;
  KernelManager &operator=(KernelManager const &) = delete;
};

} // namespace ejc

#endif // e686e5e5a21a52e79afd8eabc64d64ef458ff95c
