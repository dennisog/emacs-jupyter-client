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

extern "C" {
#include <emacs-module.h>
}

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
using std::shared_ptr;

// quick and dirty typedef for a message buffer
typedef std::vector<char> raw_message;

//
// some tools to help
//
class Json2Buf {
public:
  Json2Buf();
  raw_message operator()(Json::Value &val);

private:
  std::stringstream s;
  unique_ptr<Json::StreamWriter> sw;
};
class ISO8601 {
public:
  string operator()();

private:
  std::stringstream s;
};

extern boost::uuids::random_generator uuidgen;
extern Json2Buf json2buf;
extern ISO8601 now;

// constants
extern const raw_message msg_delim;

struct Header {
  typedef unique_ptr<Header> uptr;
  string msg_id;
  string username;
  string session;
  string date;
  string msg_type;
  string version = "5.0";

  Header(string msg_type, string username, uuid sessionid);
  Header(Json::Value &json);
  raw_message serialize();
  emacs_value toLisp(emacs_env *env);
};

// I am not sure we need this
struct Metadata {
  typedef unique_ptr<Metadata> uptr;
  virtual raw_message serialize();
  virtual emacs_value toLisp(emacs_env *env);
};

struct Content {
  typedef unique_ptr<Content> uptr;
  virtual raw_message serialize() = 0;
  virtual emacs_value toLisp(emacs_env *env) = 0;
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
  emacs_value toLisp(emacs_env *env);
};

struct ExecuteReply : public Content {
  typedef unique_ptr<ExecuteReply> uptr;
  enum class Status { ok, error, abort };
  Status status;
  unique_ptr<usr_exprs> user_expressions;
  int execution_count;

  ExecuteReply(Json::Value &json);
  emacs_value toLisp(emacs_env *env);
  raw_message serialize();
};

struct Buffers {
  typedef unique_ptr<Buffers> uptr;
  vector<raw_message> data;
};

struct Message {
  typedef unique_ptr<Message> uptr;
  typedef shared_ptr<Message> sptr;
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
  Message()
      : header(nullptr), parent_header(nullptr), metadata(nullptr),
        content(nullptr), buffers(nullptr) {}
};

// convenience typedef
typedef Message::uptr uptr;
typedef Message::sptr sptr;
} // namespace msg
} // namespace ejc

#endif // e600014baed453c6019ddedfb724cf6cfe3655a9
