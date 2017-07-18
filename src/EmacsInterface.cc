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
                              emacs_value args[], void *data) {
  return env->make_integer(env, 42);
}

//
// The Emacs interface
//
extern "C" {
// create and connect
emacs_value ejc_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data) {
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
                           void *data) {
  try {
    auto conn = ejc::get_client(env, args[0]);
    delete conn;
    return ejc::t(env);
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
int emacs_module_init(struct emacs_runtime *ert) {
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

  //
  // export this as a module to emacs
  //
  provide(env, "libemacs-jupyter-client");
  return 0;
}
} // extern "C"
