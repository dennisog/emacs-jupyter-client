//
// EmacsInterface.cc
//
// Copyright (c) 2017 Dennis Ogbe
//
// Some code (bind_function(...) and provide(...)) ripped from
// http://diobla.info/blog-archive/modules-tut.html

#include "EmacsInterface.hpp"

// Emacs modules need to define this symbol
int plugin_is_GPL_compatible;

// A debug function which does nothing but give us the answer to everything.
extern "C" emacs_value get_42(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) noexcept {
  return env->make_integer(env, 42);
}

//
// The Emacs interface
//
extern "C" {
// create and connect
emacs_value ejc_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data) noexcept {
  if (env->vec_size(env, args[0]) != 3) {
    return ejc::nil(env);
  }
  try {
    auto ks = ejc::get_vector<std::string>(env, args[0]);
    auto fn = ejc::get_string(env, args[1]);
    auto client =
        new ejc::JupyterClient(ejc::KernelSpec(ks[0], ks[1], ks[2]), fn);
    client->manager().connect();
    return env->make_user_ptr(env, ejc::JupyterClient::del,
                              static_cast<void *>(client));
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

// disconnect and destroy
emacs_value ejc_disconnect(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                           void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    delete client;
    return ejc::t(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

// is the kernel alive?
emacs_value ejc_kernel_alive(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    return client->manager().alive() ? ejc::t(env) : ejc::nil(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

//
// client -> kernel messages
//
emacs_value ejc_execute_request(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto code = ejc::get_string(env, args[1]);
    // deal with the arguments
    // FIXME
    bool silent, store_history, allow_stdin, stop_on_error;
    switch (nargs) {
    case 2:
      silent = false;
      store_history = false;
      allow_stdin = false;
      stop_on_error = false;
      break;
    case 3:
      silent = env->is_not_nil(env, args[2]);
      store_history = false;
      allow_stdin = false;
      stop_on_error = false;
      break;
    case 4:
      silent = env->is_not_nil(env, args[2]);
      store_history = env->is_not_nil(env, args[3]);
      allow_stdin = false;
      stop_on_error = false;
      break;
    case 5:
      silent = env->is_not_nil(env, args[2]);
      store_history = env->is_not_nil(env, args[3]);
      allow_stdin = env->is_not_nil(env, args[4]);
      stop_on_error = false;
      break;
    case 6:
      silent = env->is_not_nil(env, args[2]);
      store_history = env->is_not_nil(env, args[3]);
      allow_stdin = env->is_not_nil(env, args[4]);
      stop_on_error = env->is_not_nil(env, args[5]);
      break;
    }

    // send an execute_request on the shell channel
    auto hdr = client->send<ejc::msg::ExecuteRequest>(
        client->manager().shell(), code, silent, store_history, nullptr,
        allow_stdin, stop_on_error);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_inspect_request(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto code = ejc::get_string(env, args[1]);
    auto cursor_pos = env->extract_integer(env, args[2]);
    auto detail_level = env->extract_integer(env, args[3]);
    auto hdr = client->send<ejc::msg::InspectRequest>(
        client->manager().shell(), code, cursor_pos, detail_level);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_complete_request(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto code = ejc::get_string(env, args[1]);
    auto cursor_pos = env->extract_integer(env, args[2]);
    auto hdr = client->send<ejc::msg::CompleteRequest>(
        client->manager().shell(), code, cursor_pos);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

// FIXME there might be standard arguments here.
emacs_value ejc_history_request(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto output = env->is_not_nil(env, args[1]);
    auto raw = env->is_not_nil(env, args[2]);
    auto hist_access_type = ejc::get_string(env, args[3]);
    auto session = env->extract_integer(env, args[4]);
    auto start = env->extract_integer(env, args[5]);
    auto stop = env->extract_integer(env, args[6]);
    auto n = env->extract_integer(env, args[7]);
    auto pattern = ejc::get_string(env, args[8]);
    auto unique = env->is_not_nil(env, args[9]);
    auto hdr = client->send<ejc::msg::HistoryRequest>(
        client->manager().shell(), output, raw, hist_access_type, session,
        start, stop, n, pattern, unique);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_is_complete_request(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto code = ejc::get_string(env, args[1]);
    auto hdr = client->send<ejc::msg::IsCompleteRequest>(
        client->manager().shell(), code);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_kernel_info_request(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto hdr =
        client->send<ejc::msg::KernelInfoRequest>(client->manager().shell());
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_shutdown_request(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto restart = env->is_not_nil(env, args[1]);
    auto hdr = client->send<ejc::msg::ShutdownRequest>(
        client->manager().control(), restart);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

emacs_value ejc_input_reply(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->manager().alive()) {
      return ejc::make_string(env, "Execute: Kernel is dead.");
    }
    auto value = ejc::get_string(env, args[1]);
    auto hdr =
        client->send<ejc::msg::InputReply>(client->manager().stdin(), value);
    return hdr->toLisp(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

//
// get the contents of the message queue
//
emacs_value ejc_flush_queue(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    auto lisp_msg = ejc::nil(env);
    Json::Value msg;
    while (client->queue().tryPop(msg))
      lisp_msg = ejc::cons(env, ejc::json2lisp(env, msg), lisp_msg);
    return lisp_msg;
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}
} // extern "C"

//
// Emacs housekeeping
//

extern "C" {
// Bind NAME to FUN
void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  /* Prepare the arguments array */
  emacs_value args[] = {Qsym, Sfun};

  /* Make the call (2 == nb of arguments) */
  env->funcall(env, Qfset, 2, args);
}

// Provide FEATURE to Emacs
void provide(emacs_env *env, const char *feature) {
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

// initialize the module
int emacs_module_init(struct emacs_runtime *ert) noexcept {
  emacs_env *env = ert->get_environment(ert);

  //
  // client connection
  //

  // create and connect
  bind_function(
      env, "ejc/connect",
      env->make_function(
          env, 2, 2, ejc_connect,
          "(ejc/connect KERNELSPEC CONNECTION-FILE-NAME)\n\n"
          "Connect to a jupyter kernel.\n"
          "KERNELSPEC is a list of strings: (ARGV DISPLAY-NAME LANGUAGE)\n"
          "CONNECTION-FILE-NAME is the path to the file containing the "
          "connection information.\n\n"
          "Returns a user_ptr to a JupyterClient object.",
          NULL));

  // disconnect and destroy
  bind_function(env, "ejc/disconnect",
                env->make_function(env, 1, 1, ejc_disconnect,
                                   "(ejc/disconnect CLIENT-PTR)\n\n"
                                   "Disconnect the CLIENT and destroy it.\n\n"
                                   "Returns `t' if destruction was successful, "
                                   "otherwise returns string containing the "
                                   "exception message.",
                                   NULL));

  // is the kernel alive?
  bind_function(env, "ejc/alive?",
                env->make_function(env, 1, 1, ejc_kernel_alive,
                                   "(ejc/alive? CLIENT-PTR)\n\n"
                                   "Is the client CLIENT-PTR alive?.\n\n"
                                   "Returns `t' if yes, `nil' if no.",
                                   NULL));

  //
  // client -> kernel messages
  //
  bind_function(env, "ejc/execute-request",
                env->make_function(env, 2, 6, ejc_execute_request,
                                   "(ejc/execute-request CLIENT-PTR CODE "
                                   "&optional SILENT STORE-HISTORY ALLOW-STDIN "
                                   "STOP-ON-ERROR)\n\n"
                                   "Execute CODE using CLIENT-PTR\n\n"
                                   "For info on the optional arguments, check "
                                   "out the Jupyter message protocol spec.",
                                   NULL));

  bind_function(
      env, "ejc/inspect-request",
      env->make_function(
          env, 4, 4, ejc_inspect_request,
          "(ejc/inspect-request CLIENT-PTR CODE CURSOR-POS DETAIL-LEVEL)\n\n"
          "FIXME: docs.\n",
          NULL));

  bind_function(
      env, "ejc/complete-request",
      env->make_function(env, 3, 3, ejc_complete_request,
                         "(ejc/complete-request CLIENT-PTR CODE CURSOR-POS)\n\n"
                         "FIXME: docs.\n",
                         NULL));

  bind_function(env, "ejc/history-request",
                env->make_function(env, 10, 10, ejc_history_request,
                                   "(ejc/history-request CLIENT-PTR OUTPUT RAW "
                                   "HIST-ACCESS-TYPE SESSION START STOP N "
                                   "PATTERN UNIQUE)\n\n"
                                   "FIXME: docs.\n",
                                   NULL));

  bind_function(
      env, "ejc/is-complete-request",
      env->make_function(env, 2, 2, ejc_is_complete_request,
                         "(ejc/is-complete-request CLIENT-PTR CODE)\n\n"
                         "FIXME: docs.\n",
                         NULL));

  bind_function(env, "ejc/kernel-info-request",
                env->make_function(env, 1, 1, ejc_kernel_info_request,
                                   "(ejc/kernel-info-request CLIENT-PTR)\n\n"
                                   "FIXME: docs.\n",
                                   NULL));

  bind_function(
      env, "ejc/shutdown-request",
      env->make_function(env, 2, 2, ejc_shutdown_request,
                         "(ejc/shutdown-request CLIENT-PTR RESTART)\n\n"
                         "FIXME: docs.\n",
                         NULL));

  bind_function(env, "ejc/input-reply",
                env->make_function(env, 2, 2, ejc_input_reply,
                                   "(ejc/input-reply CLIENT-PTR VALUE)\n\n"
                                   "FIXME: docs.\n",
                                   NULL));

  //
  // get the contents of the message queue
  //
  bind_function(env, "ejc/flush-queue",
                env->make_function(env, 1, 1, ejc_flush_queue,
                                   "(ejc/flush-queue CLIENT-PTR)\n\n"
                                   "Return the contents of CLIENT-PTR's "
                                   "message queue as Lisp object.\n\n"
                                   "Each element in the resulting list "
                                   "contains one message. Dictionaries are "
                                   "implemented as plists.",
                                   NULL));

  //
  // export this as a module to emacs
  //
  provide(env, "libemacs-jupyter-client");
  return 0;
}
} // extern "C"

namespace ejc {
//
// convert a JSON object to a Lisp object. Arrays become lists, dicts become
// alists
//
emacs_value json2lisp(emacs_env *env, Json::Value const &object) {
  switch (object.type()) {
  case Json::nullValue:
    return ejc::nil(env);
  case Json::booleanValue:
    return object.asBool() ? ejc::t(env) : ejc::nil(env);
  case Json::intValue:
    return env->make_integer(env, object.asInt());
  case Json::uintValue:
    return env->make_integer(env, object.asUInt());
  case Json::realValue:
    return env->make_float(env, object.asDouble());
  case Json::stringValue:
    return ejc::make_string(env, object.asString());
  case Json::arrayValue: {
    auto list = ejc::nil(env);
    for (int i = object.size() - 1; i >= 0; --i)
      list = ejc::cons(env, json2lisp(env, object[i]), list);
    return list;
  }
  case Json::objectValue:
    if (object.empty()) {
      return ejc::nil(env);
    } else {
      auto keys = object.getMemberNames();
      auto list = ejc::nil(env);
      for (auto const &key : keys)
        list = ejc::cons(env, ejc::cons(env, env->intern(env, key.c_str()),
                                        json2lisp(env, object[key])),
                         list);
      return list;
    }
  default:
    return ejc::nil(env);
  }
}
} // namespace ejc
