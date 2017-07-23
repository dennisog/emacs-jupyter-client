//
// JupyterClient.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "JupyterClient.hpp"

#include <boost/format.hpp>

#include <exception>
#include <fstream>

namespace ejc {

// debug dummy handler
void print_message_received(std::vector<raw_message> msgs) { return; }

// this sucks, need to redo initialization
JupyterClient::JupyterClient(KernelSpec const &kspec,
                             std::string const &connection_file)
    : sessionid_(msg::uuidgen()), kspec_(kspec),
      cparams_(parse_connection_file_(connection_file)), queue_(100),
      hmac_(cparams_.key), shell_handler_(cparams_.key, queue_),
      iopub_handler_(cparams_.key, queue_,
                     [this](auto s) { manager().status(s); }),
      km_(cparams_, 1, print_message_received,
          [this](auto msgs) { shell_handler_(msgs); }, print_message_received,
          [this](auto msgs) { iopub_handler_(msgs); }) {}

//
// This is a quick hack to get a finalizer in the emacs C interface
//
void JupyterClient::del(void *client) noexcept {
  delete static_cast<JupyterClient *>(client);
}

//
// client stuff
//

// serialize a message to a buffer of multipart message parts
std::vector<raw_message> JupyterClient::serialize_(msg::uptr m) {
  std::vector<raw_message> to_send;
  static const raw_message empty_dict = {'{', '}'};
  // get the serialized bytes of the message
  if (m->header != nullptr) {
    to_send.push_back(m->header->serialize());
  } else {
    to_send.push_back(empty_dict);
  }
  if (m->parent_header != nullptr) {
    to_send.push_back(m->parent_header->serialize());
  } else {
    to_send.push_back(empty_dict);
  }
  if (m->metadata != nullptr) {
    to_send.push_back(m->metadata->serialize());
  } else {
    to_send.push_back(empty_dict);
  }
  if (m->content != nullptr) {
    to_send.push_back(m->content->serialize());
  } else {
    to_send.push_back(empty_dict);
  }
  // FIXME looks like we do not need to prepend the 'ident'?
  std::vector<raw_message> msgs;
  msgs.push_back(msg::msg_delim);
  msgs.push_back(hmac_.hexdigest(to_send));
  for (auto &buf : to_send)
    msgs.push_back(std::move(buf));
  if (m->buffers != nullptr)
    for (auto &buf : m->buffers->data)
      msgs.push_back(std::move(buf));
  return msgs;
}

const ConnectionParams
JupyterClient::parse_connection_file_(std::string const &fn) {
  Json::Value dict;
  std::ifstream infile(fn);
  infile >> dict;
  infile.close();
  return ConnectionParams(
      {.ip = boost::asio::ip::address_v4::from_string(dict["ip"].asString()),
       .key = dict["key"].asString(),
       .transport = dict["transport"].asString(),
       .control_port = dict["control_port"].asUInt(),
       .shell_port = dict["shell_port"].asUInt(),
       .stdin_port = dict["stdin_port"].asUInt(),
       .hb_port = dict["hb_port"].asUInt(),
       .iopub_port = dict["iopub_port"].asUInt(),
       .signature_scheme = ([](auto in) {
         if (in.compare("hmac-sha256") == 0) {
           return ConnectionParams::SignatureScheme::HMAC_SHA256;
         } else {
           throw std::runtime_error(
               (boost::format("Unsupported Signature scheme: %s") % in).str());
         }
       })(dict["signature_scheme"].asString())});
}

} // namespace ejc
