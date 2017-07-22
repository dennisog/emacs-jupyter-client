//
// Handlers.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e2877d8d75687c10978ce99d99a38100c48fd50f
#define e2877d8d75687c10978ce99d99a38100c48fd50f

#include "Crypto.hpp"
#include "KernelManager.hpp"
#include "Message.hpp"

#include <csignal>
#include <vector>

#include <boost/core/noncopyable.hpp>
#include <json/json.h>

namespace ejc {
namespace handlers {

//
// Handler base class: commonalities between all handlers
//
class Handler : private boost::noncopyable {
public:
  Handler(std::string const &hmac_key, Messagequeue &queue);
  virtual void operator()(std::vector<msg::raw_message> msgs) = 0;

protected:
  // each handler gets its own HMAC object and JSON parser
  crypto::HMAC_SHA256 hmac_;
  std::unique_ptr<Json::CharReader> parser_;
  // we keep a ref to the queue we need to push msg::uptrs on
  Messagequeue &queue_;
  // handlers can notify emacs after they have handled a message
  void notify_emacs_();
  // since every handler has to parse the headers and verify the checksums, we
  // make this a method of the base class
  Json::Value process_headers_(std::vector<msg::raw_message>::iterator &it,
                               std::vector<msg::raw_message>::iterator end);
};

// Handle messages from the shell channel
class ShellHandler : public Handler {
public:
  ShellHandler(std::string const &hmac_key, Messagequeue &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

// Handle messages from the iopub channel
class IOPubHandler : public Handler {
public:
  IOPubHandler(std::string const &hmac_key, Messagequeue &queue,
               std::function<void(KernelManager::Status)> update_status)
      : Handler(hmac_key, queue), update_status_(update_status) {}
  void operator()(std::vector<msg::raw_message> msgs);

private:
  std::function<void(KernelManager::Status)> update_status_;
};

// Handler message from the control channel
class ControlHandler : public Handler {
public:
  ControlHandler(std::string const &hmac_key, Messagequeue &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

// Handle messages from the stdin channel
class StdinHandler : public Handler {
public:
  StdinHandler(std::string const &hmac_key, Messagequeue &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

} // namespace handlers
} // namespace ejc

#endif // e2877d8d75687c10978ce99d99a38100c48fd50f
