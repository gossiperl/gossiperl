-ifndef(_gossip_types_included).
-define(_gossip_types_included, yeah).

%% struct digestEnvelope

-record(digestEnvelope, {payload_type :: string() | binary(),
                         bin_payload :: string() | binary(),
                         id :: string() | binary()}).

%% struct digestForwardedAck

-record(digestForwardedAck, {name :: string() | binary(),
                             reply_id :: string() | binary(),
                             secret :: string() | binary()}).

%% struct digestError

-record(digestError, {name :: string() | binary(),
                      heartbeat :: integer(),
                      error_code :: integer(),
                      error_entity :: string() | binary(),
                      error_entity_name :: string() | binary(),
                      error_message :: string() | binary(),
                      reply_id :: string() | binary()}).

%% struct digestExit

-record(digestExit, {name :: string() | binary(),
                     heartbeat :: integer(),
                     secret :: string() | binary()}).

%% struct digestMember

-record(digestMember, {member_name :: string() | binary(),
                       member_ip :: string() | binary(),
                       member_port :: integer(),
                       member_heartbeat :: integer()}).

%% struct digestSubscription

-record(digestSubscription, {event_type :: string() | binary(),
                             member_name :: string() | binary(),
                             origin :: string() | binary(),
                             heartbeat :: integer()}).

%% struct digest

-record(digest, {name :: string() | binary(),
                 port :: integer(),
                 heartbeat :: integer(),
                 id :: string() | binary(),
                 secret :: string() | binary()}).

%% struct digestAck

-record(digestAck, {name :: string() | binary(),
                    heartbeat :: integer(),
                    reply_id :: string() | binary(),
                    membership = [] :: list()}).

%% struct digestSubscriptions

-record(digestSubscriptions, {name :: string() | binary(),
                              heartbeat :: integer(),
                              reply_id :: string() | binary(),
                              subscriptions = [] :: list()}).

%% struct digestSubscribe

-record(digestSubscribe, {name :: string() | binary(),
                          heartbeat :: integer(),
                          id :: string() | binary(),
                          event_types = [] :: list(),
                          secret :: string() | binary()}).

%% struct digestUnsubscribe

-record(digestUnsubscribe, {name :: string() | binary(),
                            heartbeat :: integer(),
                            id :: string() | binary(),
                            event_types = [] :: list(),
                            secret :: string() | binary()}).

%% struct digestSubscribeAck

-record(digestSubscribeAck, {heartbeat :: integer(),
                             reply_id :: string() | binary(),
                             event_types = [] :: list()}).

%% struct digestUnsubscribeAck

-record(digestUnsubscribeAck, {heartbeat :: integer(),
                               reply_id :: string() | binary(),
                               event_types = [] :: list()}).

%% struct digestEvent

-record(digestEvent, {event_type :: string() | binary(),
                      event_object :: string() | binary(),
                      heartbeat :: integer()}).

-endif.
