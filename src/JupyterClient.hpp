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
  // messages
  // all messages return the mesg_id
  //
  const std::string execute_code(std::string const &code, bool silent = false,
                                 bool store_history = false,
                                 bool allow_stdin = true,
                                 bool stop_on_error = true);

  const std::string execute_user_expr(msg::usr_exprs user_expressions,
                                      bool silent = false,
                                      bool store_history = false,
                                      bool allow_stdin = true,
                                      bool stop_on_error = true);

  // const std::string inspect_request(std::string const &code, int cursor_pos,
  //                                   int detail_level);

  // const std::string complete_request(std::string const &code, int
  // cursor_pos);

  // const std::string history_request(bool output, bool raw,
  //                                   std::string const &hist_access_type,
  //                                   int session, int start, int stop, int n,
  //                                   std::string const &pattern, bool unique);

  // const std::string is_complete_request(std::string const &code);

  // const std::string kernel_info_request();

  // const std::string shutdown_request(bool restart);

  // const std::string input_reply(string const &value);

  // template<typename T>
  // const std::string send()

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

  // The connection to the kernel
  KernelManager km_;

  // serialize a message to a buffer of multipart message parts
  std::vector<raw_message> serialize_(msg::uptr);
};

} // namespace ejc

#endif // e067987744dae1b61d9d4099702a42c702346337
