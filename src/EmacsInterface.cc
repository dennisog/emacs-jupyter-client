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
    client->connect();
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
    return client->alive() ? ejc::t(env) : ejc::nil(env);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

// execute a string of code
emacs_value ejc_execute_code(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);
    if (!client->alive()) {
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
    auto id = client->execute_code(code, silent, store_history, allow_stdin,
                                   stop_on_error);
    return ejc::make_string(env, id);
  } catch (std::exception &e) {
    return env->make_string(env, e.what(), std::strlen(e.what()));
  }
}

// get the contents of the message queue
emacs_value ejc_flush_queue(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) noexcept {
  try {
    auto client = ejc::get_client(env, args[0]);

    auto lisp_msg = ejc::nil(env);
    // this works
    // auto t1 = ejc::make_string(env, "lalala");
    // auto t2 = ejc::make_string(env, "lululu");
    // return ejc::cons(env, t1, t2);
    // this works
    // lisp_msg = ejc::cons(env, t1, lisp_msg);
    // lisp_msg = ejc::cons(env, t2, lisp_msg);
    // this works as well
    // msg.append("lala");
    // msg.append("lele");
    // return ejc::json2lisp(env, msg);
    // while (client->queue().tryPop(msg)) {
    //   lisp_msg = ejc::cons(env, ejc::json2lisp(env, msg), lisp_msg);
    // }
    bool more = true;
    while (more) {
      Json::Value msg;
      more = client->queue().tryPop(msg);
      if (!more)
        break;
      lisp_msg = ejc::cons(env, ejc::json2lisp(env, msg), lisp_msg);
    }
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

  // the debug function
  bind_function(
      env, "ejc/get-42",
      env->make_function(env, 0, 0, get_42, "Get the Number 42.", NULL));

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

  // execute a string of code
  bind_function(env, "ejc/execute-code",
                env->make_function(env, 2, 6, ejc_execute_code,
                                   "(ejc/execute-code CLIENT-PTR CODE "
                                   "&optional SILENT STORE-HISTORY ALLOW-STDIN "
                                   "STOP-ON-ERROR)\n\n"
                                   "Execute CODE using CLIENT-PTR\n\n"
                                   "For info on the optional arguments, check "
                                   "out the Jupyter message protocol spec.",
                                   NULL));

  // get the contents of the message queue
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
    for (Json::ArrayIndex i = 0; i < object.size(); ++i)
      list = ejc::cons(env, json2lisp(env, object[i]), list);
    return list;
  }
  case Json::objectValue:
    if (object.empty()) {
      return ejc::nil(env);
    } else {
      auto keys = object.getMemberNames();
      // We can also do an alist with the cons function
      auto list = ejc::nil(env);
      for (auto const &key : keys)
        list = ejc::cons(env, ejc::cons(env, env->intern(env, key.c_str()),
                                        json2lisp(env, object[key])),
                         list);
      // auto plist = init_plist(env, keys[0], json2lisp(env, object[keys[0]]));
      // for (size_t i = 1; i < keys.size(); ++i)
      //   plist = plist_add(env, keys[i], json2lisp(env, object[keys[i]]));
      // return plist;
    }
  default:
    return ejc::nil(env);
  }
}
} // namespace ejc
