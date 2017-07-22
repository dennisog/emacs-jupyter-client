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
Handler::Handler(std::string const &hmac_key, Messagequeue &queue)
    : hmac_(hmac_key), parser_(getJSONparser()), queue_(queue) {}
void Handler::notify_emacs_() { std::raise(SIGUSR1); }

// parse the headers, verify the checksum, and return a half-initialized message
// object. not the prettiest way of doing things, but this works.
Json::Value
Handler::process_headers_(std::vector<msg::raw_message>::iterator &it,
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
    throw std::runtime_error("No message delimiter found");
  // after the delim, there are four dicts which we need to verify
  bool verified = hmac_.hexverify(it + 1, it + 5, *it);
  if (!verified)
    throw std::runtime_error(
        "Failed HMAC verification"); // what should we do here?
  // else lets parse the headers from this
  Json::Value hdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &hdr_json, NULL);
  Json::Value phdr_json;
  ++it;
  parser_->parse(it->data(), it->data() + it->size(), &phdr_json, NULL);
  Json::Value out;
  out.append(hdr_json);
  out.append(phdr_json);
  return out;
}

// Handle messages from the shell channel
void ShellHandler::operator()(std::vector<msg::raw_message> msgs) {
  try {
    // the verification and header parsing is done in process_headers. We will
    // re-use this iterator after process_headers is done.
    auto it = begin(msgs);
    auto msg = process_headers_(it, end(msgs));
    // get any metadata
    Json::Value metadata;
    ++it;
    parser_->parse(it->data(), it->data() + it->size(), &metadata, NULL);
    msg.append(metadata);
    // get the content
    Json::Value content;
    ++it;
    parser_->parse(it->data(), it->data() + it->size(), &content, NULL);
    msg.append(content);
    // we are done; push the JSON array on the queue and tell Emacs we got
    // something
    queue_.push(msg);
    notify_emacs_();
  } catch (std::exception &ex) {
    return;
  }
}

} // namespace handlers
} // namespace ejc
