//
// Crypto.hpp
//
// Copyright (c) 2017 Dennis Ogbe

//
// straw man HMAC_SHA256 and other stuff
//
extern "C" {
#include <gcrypt.h>
}

#include <cstdint>
#include <string>
#include <vector>

namespace ejc {
namespace crypto {

using std::vector;

typedef uint8_t bytes;

// TODO: implement this
class HMAC_SHA256 {
public:
  HMAC_SHA256(std::string const &key);
  ~HMAC_SHA256();

private:
  vector<bytes> key_;
};

} // namespace ejc
} // namespace crypto
