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

// parse the headers, verify the checksum, and return a half-initialized message
// object. not the prettiest way of doing things, but this works.
void Handler::process_headers_(msg::Message *msg,
                               std::vector<msg::raw_message>::iterator &it,
                               std::vector<msg::raw_message>::iterator end) {
  // ignore anything until the delimiter
  while (it != end) {
    if (*it == msg::msg_delim) {
      ++it;
      break;
    }
    ++it;
  }
  if (it == end)
    throw std::runtime_error("No message delimiter found!");
  // after the delim, there are four dicts which we need to verify
  bool verified = hmac_.hexverify(it + 1, it + 5, *it);
  if (!verified)
    return; // what should we do here?
  // else lets parse the headers from this
  Json::Value hdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &hdr_json, NULL);
  msg->header = std::make_unique<msg::Header>(hdr_json);
  Json::Value phdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &phdr_json, NULL);
  msg->parent_header = std::make_unique<msg::Header>(phdr_json);
}

// Handle messages from the shell channel
void ShellHandler::operator()(std::vector<msg::raw_message> msgs) {
  // the verification and header parsing is done in process_headers. We will
  // re-use this iterator after process_headers is done.
  auto it = begin(msgs);
  auto msg = new msg::Message();
  try {
    process_headers_(msg, it, end(msgs));
  } catch (std::exception &ex) {
    return;
  }
  if (msg->header == nullptr)
    return;
  // we ignore metadata for now
  ++it;
  // we now check what type of message arrived
  try {
    if (msg->header->msg_type.compare("execute_reply") == 0) {
      // parse an execute_reply
      Json::Value execute_reply_json;
      ++it;
      parser_->parse(it->data(), it->data() + it->size(), &execute_reply_json,
                     NULL);
      msg->content = std::make_unique<msg::ExecuteReply>(execute_reply_json);
    } else {
      return;
    }
    // TODO add all other options
  } catch (std::exception &ex) {
    return;
  }
  // we also ignore any buffers for now
  // now push the msg on the queue and tell emacs we have work
  queue_.push(std::shared_ptr<msg::Message>(msg));
  notify_emacs_();
}

} // namespace handlers
} // namespace ejc
