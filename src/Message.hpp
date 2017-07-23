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

// https://jupyter-client.readthedocs.io/en/latest/messaging.html#execute
typedef unordered_map<string, string> usr_exprs;

//
// header
//
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
};

//
// small interfaces for metadata, content, & buffers
//
struct Metadata {
  typedef unique_ptr<Metadata> uptr;
  virtual raw_message serialize() = 0;
};
struct Content {
  typedef unique_ptr<Content> uptr;
  virtual raw_message serialize() = 0;
  virtual const string msg_type() = 0;
};
struct Buffers {
  typedef unique_ptr<Buffers> uptr;
  vector<raw_message> data;
};

//
// Messages on shell and control channel
//
// Each message has a corresponding *_reply method which does not matter to us
// here since we pass all replies as-is to emacs unless a handler consumes them.
//

//
// execute_request
//
struct ExecuteRequest : public Content {
  typedef unique_ptr<ExecuteRequest> uptr;
  const string msg_type() { return "execute_request"; }

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

//
// inspect_request
//
struct InspectRequest : public Content {
  typedef unique_ptr<InspectRequest> uptr;
  const string msg_type() { return "inspect_request"; }

  string code;
  int cursor_pos;
  int detail_level;

  InspectRequest(string const &code, int cursor_pos, int detail_level);
  raw_message serialize();
};

//
// complete_request
//
struct CompleteRequest : public Content {
  typedef unique_ptr<CompleteRequest> uptr;
  const string msg_type() { return "complete_request"; }

  string code;
  int cursor_pos;

  CompleteRequest(string const &code, int cursor_pos);
  raw_message serialize();
};

//
// history_request
//
struct HistoryRequest : public Content {
  typedef unique_ptr<HistoryRequest> uptr;
  const string msg_type() { return "history_request"; }

  bool output;
  bool raw;
  string hist_access_type;
  int session;
  int start;
  int stop;
  int n;
  string pattern;
  bool unique;

  HistoryRequest(bool output, bool raw, string const &hist_access_type,
                 int session, int start, int stop, int n, string const &pattern,
                 bool unique);
  raw_message serialize();
};

//
// is_complete_request
//
struct IsCompleteRequest : public Content {
  typedef unique_ptr<IsCompleteRequest> uptr;
  const string msg_type() { return "is_complete_request"; }

  string code;

  IsCompleteRequest(string const &code);
  raw_message serialize();
};

//
// kernel_info_request
//
struct KernelInfoRequest : public Content {
  typedef unique_ptr<KernelInfoRequest> uptr;
  const string msg_type() { return "kernel_info_request"; }

  KernelInfoRequest();
  raw_message serialize();
};

//
// shutdown_request
//
struct ShutdownRequest : public Content {
  typedef unique_ptr<ShutdownRequest> uptr;
  const string msg_type() { return "shutdown_request"; }

  bool restart;

  ShutdownRequest(bool restart);
  raw_message serialize();
};

//
// Messages on the stdin channel
//
// The kernel sends an input_request and expects the client to respond with an
// input_reply.
//

//
// input_reply
//
struct InputReply : public Content {
  typedef unique_ptr<InputReply> uptr;
  const string msg_type() { return "input_reply"; }

  string value;

  InputReply(string const &value);
  raw_message serialize();
};

//
// Message class contains all of the information. the Jupyter client has ways to
// serialize this.
//
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
