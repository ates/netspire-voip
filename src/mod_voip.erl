-module(mod_voip).

-behaviour(gen_module).

-export([lookup_account/4, access_request/4, accounting_request/4]).

%% gen_module callbacks
-export([start/1, stop/0]).

-include("netspire.hrl").
-include("radius/radius.hrl").

-record(voip_session,
    {username, plan, active = false, balance = 0.0, amount = 0}).

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    case application:get_env(netspire, database_backend) of
        {ok, Name} ->
            Module = lists:concat([?MODULE, "_", Name]),
            gen_module:start_module(list_to_atom(Module), []);
        undefined ->
            ?ERROR_MSG("Cannot determine database backend~n", [])
    end,
    init_mnesia(),
    netspire_hooks:add(radius_acct_lookup, ?MODULE, lookup_account),
    netspire_hooks:add(radius_access_accept, ?MODULE, access_request, 10),
    netspire_hooks:add(radius_acct_request, ?MODULE, accounting_request).

init_mnesia() ->
    mnesia:create_table(voip_session, [{disc_copies, [node()]},
            {attribute, record_info(fields, voip_session)}]),
    mnesia:add_table_copy(voip_session, node(), disc_copies).

stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete_all(?MODULE).

lookup_account(_Value, _Request, UserName, _Client) ->
    Balance = 10,
    Plan = "Ultimate",
    Response = {ok, {Plan, Balance}},
    case mnesia:dirty_read(voip_session, UserName) of
        [Session] ->
            mnesia:dirty_write(Session#voip_session{active = true});
        [] ->
            Session = #voip_session{
                username = UserName,
                plan = Plan,
                balance = Balance
            },
            mnesia:dirty_write(Session)
    end,
    {stop, Response}.

access_request(Response, Request, _Extra, _Client) ->
    UserName = radius:attribute_value("User-Name", Request),
    case mnesia:dirty_read(voip_session, UserName) of
        [Session] when Session#voip_session.active == true ->
            Plan = Session#voip_session.plan,
            Balance = Session#voip_session.balance,
            Source = radius:attribute_value("Calling-Station-Id", Request),
            Destination = radius:attribute_value("Called-Station-Id", Request),
            Attrs = do_accounting(Plan, Balance, Source, Destination),
 %           {stop, Response#radius_packet{attrs = Attrs}};
            ok;
        [Session] when Session#voip_session.active == false ->
            Balance = Session#voip_session.balance,
            {stop, ok};
        [] -> {reject, []}
    end.

do_accounting(Plan, Balance, Source, Destination) ->
    case netspire_hooks:run_fold(voip_match, undef, [Plan, Source, Destination]) of
        {Cost, ConnectFee, FreeSecs} ->
           CreditTime = erlang:round(Balance / Cost) + FreeSecs 

accounting_request(_Response, ?ACCT_START, _Request, _Client) ->
    #radius_packet{code = ?ACCT_RESPONSE};
accounting_request(_Response, ?INTERIM_UPDATE, _Request, _Client) ->
    #radius_packet{code = ?ACCT_RESPONSE};
accounting_request(_Response, ?ACCT_STOP, Request, _Client) ->
    #radius_packet{code = ?ACCT_RESPONSE}.

%%
%% Internal functions
%%

%% This hack strips out Cisco's VSA duplicities in lines
%% Cisco not implemented VSA's in standard way
%% Cisco sends it's VSA attributes with the attribute name *again*
%% in the string, like:  H323-Attribute = "h323-attribute=value".
cisco_vsa_hack(Attribute, Value) ->
    case radius_dict:lookup_attribute(Attribute) of
        % Check if Vendor is Cisco or Quintum
        {attribute, {V, _}, _, _, _} when V == 9 orelse V == 6618 ->
            [{Attribute, string:join([Attribute, Value], "=")}];
        _ ->
            [{Attribute, Value}]
    end.

%% Remove attribute name from value
cisco_vsa_split(V) when is_list(V) ->
    [_, Value] = string:tokens(V, "="),
    Value.

float_to_string(Float) when is_float(Float) ->
    [Value] = io_lib:format("~.2f", [Float]),
    Value.
