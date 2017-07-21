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

// convert the header information to an emacs plist
emacs_value Header::toLisp(emacs_env *env) {
  // create a new plist
  auto plist = ejc::init_plist(env, ":msg_id", msg_id);
  // write all remaining values to the list
  plist = ejc::plist_add(env, ":username", username);
  plist = ejc::plist_add(env, ":session", session);
  plist = ejc::plist_add(env, ":date", date);
  plist = ejc::plist_add(env, ":msg_type", msg_type);
  plist = ejc::plist_add(env, ":version", version);
  return plist;
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
// execute_reply
//
ExecuteReply::ExecuteReply(Json::Value &json)
    : status([](auto j) {
        if (j.compare("ok")) {
          return Status::ok;
        } else if (j.compare("error")) {
          return Status::error;
        } else if (j.compare("abort")) {
          return Status::abort;
        } else {
          throw std::runtime_error(
              "invalid content: \"status\" field of execution reply.");
        }
      }(json["status"].asString())),
      execution_count(json["status"].asInt()) {
  if (json.isMember("user_expressions")) {
    auto uexprs_keys = json.getMemberNames();
    for (auto const &key : uexprs_keys) {
      user_expressions->emplace(key, json[key].asString());
    }
  }
}

// FIXME there is really no need for the Status enum here
emacs_value ExecuteReply::toLisp(emacs_env *env) {
  auto plist = ejc::init_plist(env, ":status", [](auto s) {
    switch (s) {
    case Status::ok:
      return "ok";
    case Status::error:
      return "error";
    case Status::abort:
      return "abort";
    }
  }(status));
  plist = ejc::plist_add(env, ":excution_count", execution_count);
  if (user_expressions != nullptr) {
    auto it = begin(*user_expressions);
    auto ue = ejc::init_plist(env, it->first, it->second);
    for (; it != end(*user_expressions); ++it)
      ue = ejc::plist_add(env, it->first, it->second);
    plist = ejc::plist_add(env, ":user_expressions", ue);
  }
  return plist;
}

raw_message ExecuteReply::serialize() { return raw_message(); }

} // namespace msg
} // namespace ejc
