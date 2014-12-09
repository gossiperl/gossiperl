namespace erl gossiperl
namespace rb gossiperl
namespace py gossiperl
namespace hs gossiperl

struct DigestEnvelope {
  1: required string payload_type;
  2: required string bin_payload;
  3: required string id;
}

struct DigestForwardedAck {
  1: required string name;
  2: required string reply_id;
  3: required string secret;
}

struct DigestError {
  1: required string name;
  2: required i64 heartbeat;
  3: required i32 error_code;
  4: required string error_entity;
  5: required string error_entity_name;
  6: required string error_message;
  7: required string reply_id;
}

struct DigestExit {
  1: required string name;
  2: required i64 heartbeat;
  3: required string secret;
}

struct DigestMember {
  1: required string member_name;
  2: required string member_ip;
  3: required i32 member_port;
  4: required i64 member_heartbeat;
}

struct DigestSubscription {
  1: required string event_type;
  2: required string member_name;
  3: required string origin;
  4: required i64 heartbeat;
}

struct Digest {
  1: required string name;
  2: required i32 port;
  3: required i64 heartbeat;
  4: required string id;
  5: required string secret;
}

struct DigestAck {
  1: required string name;
  2: required i64 heartbeat;
  3: required string reply_id;
  4: required list<DigestMember> membership;
}

struct DigestSubscriptions {
  1: required string name;
  2: required i64 heartbeat;
  3: required string reply_id;
  4: required list<DigestSubscription> subscriptions;
}

struct DigestSubscribe {
  1: required string name;
  2: required i64 heartbeat;
  3: required string id;
  4: required list<string> event_types;
  5: required string secret;
}

struct DigestUnsubscribe {
  1: required string name;
  2: required i64 heartbeat;
  3: required string id;
  4: required list<string> event_types;
  5: required string secret;
}

struct DigestSubscribeAck {
  1: required i64 heartbeat;
  2: required string reply_id;
  3: required list<string> event_types;
}

struct DigestUnsubscribeAck {
  1: required i64 heartbeat;
  2: required string reply_id;
  3: required list<string> event_types;
}

struct DigestEvent {
  1: required string event_type;
  2: required string event_object;
  3: required i64 heartbeat;
}