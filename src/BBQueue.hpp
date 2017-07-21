// -*-c++-*-
//  Copyright © 2017 Stephen Larew

#ifndef f2ff400baff475349e
#define f2ff400baff475349e

#include <condition_variable>
#include <iostream>
#include <mutex>
#include <queue>

namespace bbq {

/// A thread-safe, size-limited, humorous FIFO queue.
template <typename T> class BBQueue {
public:
  explicit BBQueue(size_t initial_size) : _available(initial_size) {
    if (!_available) {
      throw std::runtime_error("initial_size must be greater than zero");
    }
  }

  /// Pop off next item from queue. Block if queue is empty.
  T pop() {
    std::unique_lock<decltype(_mutex)> l(_mutex);
    while (_queue.empty()) {
      _condition.wait(l);
    }
    auto const f = _queue.front();
    _queue.pop();
    ++_available;
    _condition.notify_one();
    return f;
  }

  /// Pop an item off the queue without blocking.
  bool tryPop(T &f) {
    // Use try_to_lock to avoid blocking.
    std::unique_lock<decltype(_mutex)> l(_mutex, std::try_to_lock);
    if (!l || _queue.empty()) {
      return false;
    }
    f = _queue.front();
    _queue.pop();
    ++_available;
    _condition.notify_one();
    return true;
  }

  bool tryPush(T const &tee) {
    // Use try_to_lock to avoid blocking.
    std::unique_lock<decltype(_mutex)> l(_mutex, std::try_to_lock);
    if (!l || !_available) {
      return false;
    }
    _queue.push(tee);
    --_available;
    _condition.notify_one();
    return true;
  }

  void push(T const &tee) {
    std::unique_lock<decltype(_mutex)> l(_mutex);
    while (!_available) {
      _condition.wait(l);
    }
    _queue.push(tee);
    --_available;
    _condition.notify_one();
  }

private:
  std::mutex _mutex;
  std::queue<T> _queue;
  std::condition_variable _condition;
  size_t _available;
};
}

#endif
