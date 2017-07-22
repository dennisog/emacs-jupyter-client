//
// KernelManager.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "KernelManager.hpp"

#include <boost/format.hpp>

namespace ejc {

KernelManager::KernelManager(ConnectionParams &cparams, unsigned int nthreads,
                             handler_fn control_handler,
                             handler_fn shell_handler, handler_fn stdin_handler,
                             handler_fn iopub_handler)
    : ctx_(zmq::context_t(nthreads)), cparams_(cparams),
      control_chan_(ctx_, ZMQ_DEALER, control_handler),
      shell_chan_(ctx_, ZMQ_DEALER, shell_handler),
      stdin_chan_(ctx_, ZMQ_DEALER, stdin_handler),
      // FIXME timeout and interval values are hardcoded rn
      hb_chan_(ctx_, ZMQ_DEALER, std::chrono::milliseconds(500),
               std::chrono::milliseconds(3000), [this](auto a) { alive_ = a; }),
      iopub_chan_(ctx_, ZMQ_SUB, iopub_handler), alive_(false),
      status_(Status::Starting) {}

KernelManager::~KernelManager() {
  if (control_chan_.running())
    control_chan_.stop();
  if (shell_chan_.running())
    shell_chan_.stop();
  if (stdin_chan_.running())
    stdin_chan_.stop();
  if (hb_chan_.running())
    hb_chan_.stop();
  if (iopub_chan_.running())
    iopub_chan_.stop();
}

void KernelManager::connect() {
  using boost::format;
  int i = 5; // number of tries FIXME hardcoded
  for (; i > 0; --i) {
    try {
      // connct the channels to their endpoints and start them
      control_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                             cparams_.ip.to_string() % cparams_.control_port)
                                .str());
      shell_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.shell_port)
                              .str());
      stdin_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.stdin_port)
                              .str());
      hb_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                        cparams_.ip.to_string() % cparams_.hb_port)
                           .str());
      iopub_chan_.connect((format("%s://%s:%d") % cparams_.transport %
                           cparams_.ip.to_string() % cparams_.iopub_port)
                              .str());
      control_chan_.start();
      shell_chan_.start();
      stdin_chan_.start();
      hb_chan_.start();
      iopub_chan_.start();
      break;
    } catch (std::exception &ex) {
      // FIXME
      ;
    }
  }
  if (i <= 0) {
    throw std::runtime_error("Connection failure.");
  }
}

} // namespace ejc
