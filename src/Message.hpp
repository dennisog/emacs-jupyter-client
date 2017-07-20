//
// Message.hpp
//
// Copyright (c) 2017 Dennis Ogbe

#ifndef e600014baed453c6019ddedfb724cf6cfe3655a9
#define e600014baed453c6019ddedfb724cf6cfe3655a9

#include <chrono>
#include <ctime>
#include <iomanip>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <json/json.h>

namespace ejc {
namespace msg {

using std::string;
using std::vector;
using std::unordered_map;
using boost::uuids::uuid;
using std::unique_ptr;

// some tools to help
extern boost::uuids::random_generator uuidgen;

extern const std::vector<uint8_t> msg_delim;

// quick and dirty typedef for a message buffer
typedef std::vector<uint8_t> raw_message;

struct Header {
  typedef unique_ptr<Header> uptr;
  string msg_id;
  string username;
  string session;
  string date;
  string msg_type;
  string version = "5.0";

  Header(string msg_type, string username, uuid sessionid);
  raw_message serialize();
};

// I am not sure we need this
struct Metadata {
  typedef unique_ptr<Metadata> uptr;
  virtual raw_message serialize() = 0;
};

struct Content {
  typedef unique_ptr<Content> uptr;
  virtual raw_message serialize() = 0;
};

// https://jupyter-client.readthedocs.io/en/latest/messaging.html#execute

typedef unordered_map<string, string> usr_exprs;

struct ExecuteRequest : public Content {
  typedef unique_ptr<ExecuteRequest> uptr;
  string code;
  bool silent;
  bool store_history;
  unique_ptr<usr_exprs> user_expressions;
  bool allow_stdin;
  bool stop_on_error;

  ExecuteRequest(string const &code, bool silent, bool store_history,
                 unique_ptr<usr_exprs> user_expressions, bool allow_stdin,
                 bool stop_on_error);
  raw_message serialize();
};

struct Buffers {
  typedef unique_ptr<Buffers> uptr;
  vector<vector<uint8_t>> data;
};

struct Message {
  typedef unique_ptr<Message> uptr;
  unique_ptr<Header> header;
  unique_ptr<Header> parent_header;
  unique_ptr<Metadata> metadata;
  unique_ptr<Content> content;
  unique_ptr<Buffers> buffers;
  Message(unique_ptr<Header> header, unique_ptr<Header> parent_header,
          unique_ptr<Metadata> metadata, unique_ptr<Content> content,
          unique_ptr<Buffers> buffers)
      : header(std::move(header)), parent_header(std::move(parent_header)),
        metadata(std::move(metadata)), content(std::move(content)),
        buffers(std::move(buffers)) {}
};
} // namespace msg
} // namespace ejc

#endif // e600014baed453c6019ddedfb724cf6cfe3655a9
