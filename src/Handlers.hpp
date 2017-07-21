//
// Handlers.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e2877d8d75687c10978ce99d99a38100c48fd50f
#define e2877d8d75687c10978ce99d99a38100c48fd50f

#include "BBQueue.hpp"
#include "Crypto.hpp"
#include "Message.hpp"

#include <csignal>

#include <boost/core/noncopyable.hpp>
#include <json/json.h>

namespace ejc {
namespace handlers {

//
// Handler base class: commonalities between all handlers
//
class Handler : private boost::noncopyable {
public:
  Handler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue);
  virtual void operator()(std::vector<msg::raw_message> msgs) = 0;
  // FIXME this sucks but oh well
  void set_key(std::string const &hmac_key);

protected:
  // each handler gets its own HMAC object and JSON parser
  crypto::HMAC_SHA256 hmac_;
  std::unique_ptr<Json::CharReader> parser_;
  // we keep a ref to the queue we need to push msg::uptrs on
  bbq::BBQueue<msg::sptr> &queue_;
  // handlers can notify emacs after they have handled a message
  void notify_emacs_();
};

// Handle messages from the shell channel
class ShellHandler : public Handler {
public:
  ShellHandler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

// Handle messages from the iopub channel
class IOPubHandler : public Handler {
public:
  IOPubHandler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

// Handler message from the control channel
class ControlHandler : public Handler {
public:
  ControlHandler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

// Handle messages from the stdin channel
class StdinHandler : public Handler {
public:
  StdinHandler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue)
      : Handler(hmac_key, queue) {}
  void operator()(std::vector<msg::raw_message> msgs);
};

} // namespace handlers
} // namespace ejc

#endif // e2877d8d75687c10978ce99d99a38100c48fd50f
