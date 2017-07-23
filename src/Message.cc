//
// Message.cc
//
// Copyright (c) 2017 Dennis Ogbe

#include "Message.hpp"
#include "EmacsInterface.hpp"

namespace ejc {
namespace msg {

// tools

Json2Buf::Json2Buf() : sw(nullptr) {
  Json::StreamWriterBuilder builder;
  builder.settings_["indentation"] = "";
  sw.reset(builder.newStreamWriter());
}
raw_message Json2Buf::operator()(Json::Value &val) {
  s.str("");
  s.clear();
  sw->write(val, &s);
  auto str = s.str();
  raw_message out(begin(str), end(str));
  return out;
}
std::string ISO8601::operator()() {
  s.str("");
  s.clear();
  auto now =
      std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
  s << std::put_time(std::localtime(&now), "%FT%T%Z");
  return s.str();
}

boost::uuids::random_generator uuidgen = boost::uuids::random_generator();
Json2Buf json2buf = Json2Buf();
ISO8601 now = ISO8601();

//
// the message delimiter
//
const raw_message msg_delim = {'<', 'I', 'D', 'S', '|', 'M', 'S', 'G', '>'};

//
// header
//

// get a new header
Header::Header(string msg_type, string username, uuid sessionid)
    : msg_id(boost::uuids::to_string(uuidgen())), username(username),
      session(boost::uuids::to_string(sessionid)), date(now()),
      msg_type(msg_type) {}

// get the header from a json value
Header::Header(Json::Value &json)
    : msg_id(json["msg_id"].asString()), username(json["username"].asString()),
      session(json["session"].asString()), date(json["date"].asString()),
      msg_type(json["msg_type"].asString()),
      version(json["version"].asString()) {}

// serialize the header to JSON
raw_message Header::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["msg_id"] = msg_id;
  val["username"] = username;
  val["session"] = session;
  val["date"] = date;
  val["msg_type"] = msg_type;
  val["version"] = version;
  return json2buf(val);
}

//
// execute_request
//
ExecuteRequest::ExecuteRequest(std::string const &code, bool silent,
                               bool store_history,
                               std::unique_ptr<msg::usr_exprs> user_expressions,
                               bool allow_stdin, bool stop_on_error)
    : code(code), silent(silent), store_history(store_history),
      user_expressions(std::move(user_expressions)), allow_stdin(allow_stdin),
      stop_on_error(stop_on_error) {}

raw_message ExecuteRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  Json::Value uexpr(Json::ValueType::objectValue);
  val["code"] = code;
  val["silent"] = silent;
  val["store_history"] = store_history;
  val["allow_stdin"] = allow_stdin;
  val["stop_on_error"] = stop_on_error;
  if (user_expressions != nullptr)
    for (auto &e : *user_expressions)
      uexpr[e.first] = e.second;
  val["user_expressions"] = uexpr;
  return json2buf(val);
}

//
// inspect_request
//
InspectRequest::InspectRequest(string const &code, int cursor_pos,
                               int detail_level)
    : code(code), cursor_pos(cursor_pos), detail_level(detail_level) {}

raw_message InspectRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["code"] = code;
  val["cursor_pos"] = cursor_pos;
  val["detail_level"] = detail_level;
  return json2buf(val);
}

//
// complete_request
//
CompleteRequest::CompleteRequest(string const &code, int cursor_pos)
    : code(code), cursor_pos(cursor_pos) {}

raw_message CompleteRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["code"] = code;
  val["cursor_pos"] = cursor_pos;
  return json2buf(val);
}

//
// history_request
//
HistoryRequest::HistoryRequest(bool output, bool raw,
                               string const &hist_access_type, int session,
                               int start, int stop, int n,
                               string const &pattern, bool unique)
    : output(output), raw(raw), hist_access_type(hist_access_type),
      session(session), start(start), stop(stop), n(n), pattern(pattern),
      unique(unique) {}

raw_message HistoryRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["output"] = output;
  val["raw"] = raw;
  val["hist_access_type"] = hist_access_type;
  val["session"] = session;
  val["start"] = start;
  val["stop"] = stop;
  val["n"] = n;
  val["pattern"] = pattern;
  val["unique"] = unique;
  return json2buf(val);
}

//
// is_complete_request
//
IsCompleteRequest::IsCompleteRequest(string const &code) : code(code) {}

raw_message IsCompleteRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["code"] = code;
  return json2buf(val);
}

//
// kernel_info_request
//
KernelInfoRequest::KernelInfoRequest() {}

raw_message KernelInfoRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  return json2buf(val);
}

//
// shutdown_request
//
ShutdownRequest::ShutdownRequest(bool restart) : restart(restart) {}

raw_message ShutdownRequest::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["restart"] = restart;
  return json2buf(val);
}

//
// input_reply
//
InputReply::InputReply(string const &value) : value(value) {}
raw_message InputReply::serialize() {
  Json::Value val(Json::ValueType::objectValue);
  val["value"] = value;
  return json2buf(val);
}

} // namespace msg
} // namespace ejc
