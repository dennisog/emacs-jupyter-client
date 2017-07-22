//
// Channels.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "Channels.hpp"

#include <chrono>
#include <random>

namespace ejc {
//
// A communication channel to the Jupyter kernel
//
Channel::Channel(zmq::context_t &ctx, int flags,
                 std::function<void(std::vector<raw_message>)> rx_handler)
    : rx_handler_(rx_handler), sock_(ctx, flags), running_(false) {}

void Channel::connect(std::string const &endpoint) { sock_.connect(endpoint); }

bool Channel::send(raw_message &data) {
  std::lock_guard<std::mutex> lock(sockmtx_);
  return sock_.send(data);
}

void Channel::send_multipart(std::vector<raw_message> &data) {
  std::lock_guard<std::mutex> lock(sockmtx_);
  return sock_.send_multipart(data);
}

bool Channel::running() { return running_; }

void Channel::start() {
  if (!running()) {
    running_ = true;
    rx_thread_ = std::thread([this] { run(); });
  }
}

void Channel::stop() {
  running_ = false;
  rx_thread_.join();
}

void Channel::run() {
  while (true) {
    if (!running()) {
      break;
    } else {
      std::lock_guard<std::mutex> lock(sockmtx_);
      // FIXME hardcoded polling timeout
      if (sock_.pollin(5)) {
        auto msgs = sock_.recv_multipart();
        rx_handler_(msgs);
      }
    }
  }
}

// a heartbeat channel is only slightly different from a regular channel
HBChannel::HBChannel(zmq::context_t &ctx, int flags,
                     std::chrono::milliseconds timeout,
                     std::chrono::milliseconds interval,
                     std::function<void(bool)> notify_manager)
    : Channel(ctx, flags, [](auto a) {}), timeout_(timeout),
      interval_(interval), notify_manager_(notify_manager) {}

// send a 'ping' to the kernel, wait for a response and timeout if no response.
void HBChannel::run_heartbeat_() {
  using namespace std::chrono;
  std::mt19937 mt(system_clock::now().time_since_epoch().count());
  std::uniform_real_distribution<> r(0, 1);
  auto rand = [&mt, &r] { return r(mt); };

  while (true) {
    if (!running()) {
      break;
    } else {
      auto start = system_clock::now();
      sock_.send(ping_);
      {
        std::lock_guard<std::mutex> lock(sockmtx_);
        if (sock_.pollin(timeout_.count())) {
          auto buf = sock_.recv_multipart();
          notify_manager_(true);
        } else {
          notify_manager_(false);
        }
      }
      auto elapsed = duration_cast<milliseconds>(system_clock::now() - start);
      auto sleepfor = rand() * (interval_ - elapsed);
      std::this_thread::sleep_for(sleepfor);
    }
  }
}

void HBChannel::start() {
  if (!running()) {
    running_ = true;
    rx_thread_ = std::thread([this] { run_heartbeat_(); });
  }
}

} // namespace ejc
