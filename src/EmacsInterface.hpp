//
// EmacsInterface.hpp
//
// Copyright (c) 2017 Dennis Ogbe

extern "C" {
#include <emacs-module.h>
}

//
// emacs module housekeeping
//
extern "C" {
// bind NAME to FUN
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);

// provide FEATURE to emacs
void provide(emacs_env *env, const char *feature);

// initialize the module
int emacs_module_init(struct emacs_runtime *ert);
} // extern "C"
