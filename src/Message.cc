#include "Message.hpp"

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

// get a new header
Header::Header(string msg_type, string username, uuid sessionid)
    : msg_id(boost::uuids::to_string(uuidgen())), username(username),
      session(boost::uuids::to_string(sessionid)), date(now()),
      msg_type(msg_type) {}

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

// make an execute_request object
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

} // namespace msg
} // namespace ejc
