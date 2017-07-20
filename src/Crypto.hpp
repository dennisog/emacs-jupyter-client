//
// Crypto.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e5508bc184b6dad4b0b7e3a2109defc651b7c534
#define e5508bc184b6dad4b0b7e3a2109defc651b7c534
//
// straw man HMAC_SHA256 and other stuff
//
extern "C" {
#include <gcrypt.h>
}

#include "Message.hpp"

#include <cstdint>
#include <string>
#include <vector>

namespace ejc {
namespace crypto {

using std::vector;
using std::string;
using msg::raw_message;

//
// initialize the gcrypt library
//
extern bool inited;
void init();

//
// A small wrapper for the hmac-sha256 algo
//
// FIXME: we can abstract the algorithm type out and make this a general class
class HMAC_SHA256 {
public:
  // housekeeping
  HMAC_SHA256(string const &key);
  HMAC_SHA256(HMAC_SHA256 const &other);
  ~HMAC_SHA256();
  bool auth(vector<raw_message> const &msgs, raw_message &signature);
  raw_message sign(vector<raw_message> const &msgs);

private:
  vector<uint8_t> key_;
  gcry_mac_hd_t handle_;
};

} // namespace ejc
} // namespace crypto

#endif // e5508bc184b6dad4b0b7e3a2109defc651b7c534
