//
// JupyterClient.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "JupyterClient.hpp"

#include <boost/asio.hpp>
#include <boost/format.hpp>

namespace ejc {

KernelManager::KernelManager(std::string const &connection_file,
                             unsigned int nthreads)
    : connection_file_name_(connection_file), ctx_(zmq::context_t(nthreads)) {
  parse_connection_file_();
}

void KernelManager::parse_connection_file_() {}

void KernelManager::connect() {}

JupyterClient::JupyterClient(KernelSpec const &kspec,
                             std::string const &connection_file)
    : kspec_(kspec), km_(connection_file) {}

//
// This is a quick hack to get a finalizer in the emacs C interface
//
void JupyterClient::del(void *client) {
  delete static_cast<JupyterClient *>(client);
}

//
// Connection
//
void JupyterClient::connect() { km_.connect(); }

} // namespace ejc
