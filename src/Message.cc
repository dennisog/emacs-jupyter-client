#include "Message.hpp"

namespace ejc {
namespace msg {

// tools
boost::uuids::random_generator uuidgen = boost::uuids::random_generator();
std::string json2string(Json::Value &val) {
  static Json::FastWriter w;
  return w.write(val);
}

//
// the message delimiter
//
const std::vector<uint8_t> msg_delim = {'<', 'I', 'D', 'S', '|',
                                        'M', 'S', 'G', '>'};

// get a new header
Header::Header(string msg_type, string username, uuid sessionid)
    : username(username), session(boost::uuids::to_string(sessionid)),
      msg_type(msg_type) {
  // compute the date ISO 8601 FIXME
  auto now =
      std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
  std::stringstream s;
  s << std::put_time(std::localtime(&now), "%FT%T%Z");
  date = s.str();
  // compute the message id
  auto id = uuidgen();
  msg_id = boost::uuids::to_string(id);
  // set the session id
}

// serialize the header to JSON
raw_message Header::serialize() {
  Json::Value val;
  val["msg_id"] = msg_id;
  val["username"] = username;
  val["session"] = session;
  val["date"] = date;
  val["msg_type"] = msg_type;
  val["version"] = version;
  auto str = json2string(val);
  raw_message out;
  out.insert(begin(out), str.data(), str.data() + str.length());
  return out;
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
  Json::Value val;

  val["code"] = "code";
  val["silent"] = silent;
  val["store_history"] = store_history;
  val["allow_stdin"] = allow_stdin;
  val["stop_on_error"] = stop_on_error;
  if (user_expressions != nullptr) {
    Json::Value uexpr;
    for (auto &e : *user_expressions)
      uexpr[e.first] = e.second;
    val["user_expressions"] = uexpr;
  }
  auto str = json2string(val);
  raw_message out;
  out.insert(begin(out), str.data(), str.data() + str.length());
  return out;
}

} // namespace msg
} // namespace ejc
