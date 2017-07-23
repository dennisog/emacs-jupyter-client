//
// EmacsInterface.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e34f90230d2f45b894e32a381e88eddc0c6ee3a4
#define e34f90230d2f45b894e32a381e88eddc0c6ee3a4

extern "C" {
#include <emacs-module.h>
}

#include "Handlers.hpp"
#include "JupyterClient.hpp"

#include <cstring>
#include <exception>
#include <string>
#include <vector>

//
// The Emacs interface
//
extern "C" {
// connect to a kernel and return a pointer to a JupyterClient
emacs_value ejc_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data) noexcept;
// disconnect and destroy
emacs_value ejc_disconnect(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                           void *data) noexcept;
// is the kernel alive?
emacs_value ejc_kernel_alive(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) noexcept;

// execute a string of code
emacs_value ejc_execute_code(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) noexcept;

// get the contents of the message queue
emacs_value ejc_flush_queue(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) noexcept;
} // extern "C"

//
// Emacs module housekeeping
//
extern "C" {
// bind NAME to FUN
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);

// provide FEATURE to emacs
void provide(emacs_env *env, const char *feature);

// initialize the module
int emacs_module_init(struct emacs_runtime *ert) noexcept;
} // extern "C"

//
// some helpers
//
namespace ejc {
// emacs booleans
inline emacs_value nil(emacs_env *env) { return env->intern(env, "nil"); }
inline emacs_value t(emacs_env *env) { return env->intern(env, "t"); }
inline emacs_value make_string(emacs_env *env, std::string const &str) {
  return env->make_string(env, str.data(), str.length());
}
// get a string from an emacs_value
inline std::string get_string(emacs_env *env, emacs_value val) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, val, NULL, &size);
  std::vector<char> buf(size);
  env->copy_string_contents(env, val, buf.data(), &size);
  std::string str;
  str.assign(buf.data(), size - 1); // to remove the null byte
  return str;
}
// pointer cast
inline JupyterClient *get_client(emacs_env *env, emacs_value val) {
  return static_cast<ejc::JupyterClient *>(env->get_user_ptr(env, val));
}
// std::vector for all emacs types
template <typename T>
inline std::vector<T> get_vector(emacs_env *env, emacs_value vec);
template <>
inline std::vector<std::string> get_vector(emacs_env *env, emacs_value vec) {
  ptrdiff_t size = env->vec_size(env, vec);
  std::vector<std::string> out;
  for (ptrdiff_t i = 0; i < size; ++i) {
    out.push_back(get_string(env, env->vec_get(env, vec, i)));
  }
  return out;
}
template <>
inline std::vector<intmax_t> get_vector(emacs_env *env, emacs_value vec) {
  ptrdiff_t size = env->vec_size(env, vec);
  std::vector<intmax_t> out;
  for (ptrdiff_t i = 0; i < size; ++i) {
    out.push_back(env->extract_integer(env, env->vec_get(env, vec, i)));
  }
  return out;
}
template <>
inline std::vector<double> get_vector(emacs_env *env, emacs_value vec) {
  ptrdiff_t size = env->vec_size(env, vec);
  std::vector<double> out;
  for (ptrdiff_t i = 0; i < size; ++i) {
    out.push_back(env->extract_float(env, env->vec_get(env, vec, i)));
  }
  return out;
}

// cons, but in C
inline emacs_value cons(emacs_env *env, emacs_value x, emacs_value y) {
  auto Fcons = env->intern(env, "cons");
  emacs_value args[] = {x, y};
  return env->funcall(env, Fcons, 2, args);
}

// convert a JSON object to a Lisp object
emacs_value json2lisp(emacs_env *env, Json::Value const &object);
} // namespace ejc

#endif // e34f90230d2f45b894e32a381e88eddc0c6ee3a4
