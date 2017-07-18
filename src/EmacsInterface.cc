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
extern "C" emacs_value get_42(emacs_env *env, long nargs, emacs_value args[],
                              void *data) {
  return env->make_integer(env, 42);
}

//
// the emacs interface
//

// Bind NAME to FUN
extern "C" {
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
  bind_function(
      env, "ejc/get-42",
      env->make_function(env, 0, 0, get_42, "Get the Number 42.", NULL));
  provide(env, "libemacs-jupyter-client");
  return 0;
}
} // extern "C"
