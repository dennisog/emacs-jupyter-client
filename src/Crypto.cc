//
// Crypto.
//
// Copyright (c) 2017 Dennis Ogbe

#include "Crypto.hpp"

#include <boost/algorithm/hex.hpp>
#include <exception>

namespace ejc {
namespace crypto {

using std::vector;
using std::string;
using msg::raw_message;

//
// initialize the gcrypt library
//
bool inited = false;
void init() {
  if (!gcry_check_version(GCRYPT_VERSION))
    throw std::runtime_error("libgcrypt version mismatch");
  // no need for secure memory here
  gcry_control(GCRYCTL_DISABLE_SECMEM, 0);
  // finish
  gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
  inited = true;
}

//
// housekeeping
//
HMAC_SHA256::HMAC_SHA256(string const &key)
    : key_(key.data(), key.data() + key.length()) {
  // initialize gcrypt if it hasn't been already
  if (!inited)
    init();
  // store the key
  if (key.size() == 0)
    throw std::runtime_error("HMAC_SHA256: Key of size 0");
  // get the mac object
  auto err = gcry_mac_open(&handle_, GCRY_MAC_HMAC_SHA256, 0, NULL);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not initialize context");
  err = gcry_mac_setkey(handle_, static_cast<void *>(key_.data()), key_.size());
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not store key");
  err = gcry_mac_test_algo(GCRY_MAC_HMAC_SHA256);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Initialization error");
}

HMAC_SHA256::HMAC_SHA256(HMAC_SHA256 const &other)
    : key_(begin(other.key_), end(other.key_)) {

  auto err = gcry_mac_open(&handle_, GCRY_MAC_HMAC_SHA256, 0, NULL);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not initialize context");
  err = gcry_mac_setkey(handle_, static_cast<void *>(key_.data()), key_.size());
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not store key");
  err = gcry_mac_test_algo(GCRY_MAC_HMAC_SHA256);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Initialization error");
}

HMAC_SHA256::~HMAC_SHA256() {
  if (handle_) {
    gcry_mac_close(handle_);
  }
}

//
// functionality
//

inline void feed(gcry_mac_hd_t &handle, vector<raw_message> const &msgs) {
  gcry_error_t err;
  for (auto const &msg : msgs) {
    err = gcry_mac_write(handle, static_cast<const void *>(msg.data()),
                         msg.size());
    if (err)
      throw std::runtime_error(
          "HMAC_SHA256: Could not write data to algorithm");
  }
}

bool HMAC_SHA256::verify(vector<raw_message> const &msgs,
                         raw_message &signature) {
  // reset state
  auto err = gcry_mac_reset(handle_);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not reset the algorithm");
  // write input data
  feed(handle_, msgs);
  // verify the signature
  err = gcry_mac_verify(handle_, static_cast<void *>(signature.data()),
                        signature.size());
  switch (err) {
  case GPG_ERR_CHECKSUM:
    return false;
  case 0:
    return true;
  default:
    throw std::runtime_error("HMAC_SHA256: Errors verifying checksum");
  }
}

// same as verify(...) above, but message is expected to be in hex format
bool HMAC_SHA256::hexverify(vector<raw_message> const &msgs,
                            raw_message &signature) {
  size_t len = gcry_mac_get_algo_maclen(GCRY_MAC_HMAC_SHA256);
  auto hexlen = len + len;
  if (signature.size() != hexlen) {
    return false;
  }
  raw_message digest;
  boost::algorithm::unhex(begin(signature), end(signature),
                          back_inserter(digest));
  return verify(msgs, digest);
}

raw_message HMAC_SHA256::digest(vector<raw_message> const &msgs) {
  // reset state
  auto err = gcry_mac_reset(handle_);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not reset the algorithm");
  // write input data
  feed(handle_, msgs);
  // pull the signature out
  size_t len = gcry_mac_get_algo_maclen(GCRY_MAC_HMAC_SHA256);
  raw_message sig(len);
  err = gcry_mac_read(handle_, static_cast<void *>(sig.data()), &len);
  if (err)
    throw std::runtime_error("HMAC_SHA256: Could not read signature");
  return sig;
}

raw_message HMAC_SHA256::hexdigest(vector<raw_message> const &msgs) {
  auto raw = digest(msgs);
  raw_message out;
  boost::algorithm::hex(begin(raw), end(raw), back_inserter(out));
  return out;
}

} // namespace ejc
} // namespace crypto
