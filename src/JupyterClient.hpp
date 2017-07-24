//
// JupyterClient.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e067987744dae1b61d9d4099702a42c702346337
#define e067987744dae1b61d9d4099702a42c702346337

#include "BBQueue.hpp"
#include "Crypto.hpp"
#include "Handlers.hpp"
#include "KernelManager.hpp"
#include "Message.hpp"

#include <string>
#include <vector>

namespace ejc {

// there are better ways of abstracting buffers, but this works for now
// TODO, switch to asio::mutable buffers to avoid memcpy when sending zmq
// messages.
using msg::raw_message;

// https://jupyter-client.readthedocs.io/en/latest/kernels.html#kernelspecs
struct KernelSpec {
  std::string argv_;
  std::string display_name_;
  std::string language_;
  KernelSpec(std::string const &argv, std::string const &display_name,
             std::string const &language)
      : argv_(argv), display_name_(display_name), language_(language) {}
};

class JupyterClient {
public:
  JupyterClient(KernelSpec const &kspec, std::string const &connection_file);
  static void del(void *client) noexcept;

  //
  // refs
  //
  Messagequeue &queue() { return queue_; }
  KernelManager &manager() { return km_; }

  //
  // send a message of type T on the channel chan
  //
  template <typename T, typename... Args>
  msg::Header::uptr send(Channel &chan, Args &&... args) {
    using std::make_unique;
    msg::Content::uptr content =
        std::unique_ptr<T>(new T(std::forward<Args>(args)...));
    auto header =
        make_unique<msg::Header>(content->msg_type(), "emacs-user", sessionid_);
    auto ret = header->copy();
    auto msg = std::make_unique<msg::Message>(
        std::move(header), nullptr, nullptr, std::move(content), nullptr);
    // serialize & send
    auto raw_msgs = serialize_(std::move(msg));
    chan.send_multipart(raw_msgs);
    return ret;
  }

  // interpret a connection file
  static const ConnectionParams parse_connection_file_(std::string const &fn);

private:
  boost::uuids::uuid sessionid_;

  KernelSpec kspec_;
  ConnectionParams cparams_;

  Messagequeue queue_;
  crypto::HMAC_SHA256 hmac_;

  // Handlers
  handlers::ShellHandler shell_handler_;
  handlers::IOPubHandler iopub_handler_;
  // looks like we don't need specialized handlers on these channels... for now.
  handlers::ShellHandler control_handler_;
  handlers::ShellHandler stdin_handler_;

  // The connection to the kernel
  KernelManager km_;

  // serialize a message to a buffer of multipart message parts
  std::vector<raw_message> serialize_(msg::uptr);
};

} // namespace ejc

#endif // e067987744dae1b61d9d4099702a42c702346337
