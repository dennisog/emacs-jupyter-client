//
// Handlers.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "Handlers.hpp"

namespace ejc {
namespace handlers {

// we need JSON parsers
std::unique_ptr<Json::CharReader> getJSONparser() {
  static Json::CharReaderBuilder builder;
  builder["collectComments"] = false;
  auto rd = std::unique_ptr<Json::CharReader>(builder.newCharReader());
  return rd;
}

using msg::raw_message;

// Handler housekeeping
Handler::Handler(std::string const &hmac_key, bbq::BBQueue<msg::sptr> &queue)
    : hmac_(hmac_key), parser_(getJSONparser()), queue_(queue) {}
void Handler::notify_emacs_() { std::raise(SIGUSR1); }
// FIXME!!!
void Handler::set_key(const std::string &hmac_key) {
  hmac_ = ejc::crypto::HMAC_SHA256(hmac_key);
}

// Handle messages from the shell channel
void ShellHandler::operator()(std::vector<msg::raw_message> msgs) {
  // ignore anything until the delimiter
  auto it = begin(msgs);
  while (it != end(msgs)) {
    if (*it == msg::msg_delim) {
      ++it;
      break;
    }
    ++it;
  }
  if (it == end(msgs))
    throw std::runtime_error("No message delimiter found!");
  // after the delim, there are four dicts which we need to verify
  bool verified = hmac_.hexverify(it + 1, it + 5, *it);
  if (!verified)
    return; // what should we do here?
  // else, let's construct a message object from this
  auto msg = std::make_shared<msg::Message>();
  Json::Value hdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &hdr_json, NULL);
  msg->header = std::make_unique<msg::Header>(hdr_json);
  Json::Value phdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &phdr_json, NULL);
  msg->parent_header = std::make_unique<msg::Header>(phdr_json);
  // we ignore metadata for now
  ++it;
  // we now check what type of message arrived
  if (msg->header->msg_type.compare("execute_reply")) {
    // parse an execute_reply
    Json::Value execute_reply_json;
    ++it;
    parser_->parse(it->data(), it->data() + it->size(), &execute_reply_json,
                   NULL);
    msg->content = std::make_unique<msg::ExecuteReply>(execute_reply_json);
  }
  // TODO add all other options

  // now push the msg on the queue and tell emacs we have work
  queue_.push(msg);
  notify_emacs_();
}

} // namespace handlers
} // namespace ejc
