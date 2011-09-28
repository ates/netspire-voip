-module(mod_voip_pgsql).

-behaviour(gen_module).

-export([fetch_account/2, stop_session/5]).

%% gen_module callbacks
-export([start/1, stop/0]).

-include("netspire.hrl").

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    netspire_hooks:add(voip_fetch_account, ?MODULE, fetch_account),
    netspire_hooks:add(voip_stop_session, ?MODULE, stop_session).

stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete_all(?MODULE).

fetch_account(_, UserName) ->
    DbResult = execute("SELECT * FROM auth($1)", [UserName]),
    Result = process_fetch_account_result(DbResult),
    {stop, Result}.

stop_session(_, UserName, SID, Duration, FinishedAt) ->
    Q = "SELECT * FROM voip_stop_session($1, $2, $3, $4)",
    case execute(Q, [UserName, SID, Duration, calendar:now_to_universal_time(FinishedAt)]) of
        {ok, _, _} ->
            {stop, ok};
        {error, Reason} ->
            {stop, {error, Reason}}
    end.

execute(Q, Params) ->
    mod_postgresql:execute(Q, Params).

process_fetch_account_result(Result) ->
    F = fun({_, _, _, null, null}) ->
                undefined;
            ({_, _, _, Name, Value}) ->
                {binary_to_list(Name), binary_to_list(Value)}
    end,
    case Result of
        {ok, _Columns, Rows} ->
            case Rows of
                [] ->
                    undefined;
                Res ->
                    {Password, Balance, Plan, _, _} = lists:nth(1, Res),
                    Attrs = lists:filter(fun(E) -> E /= undefined end, lists:map(F, Res)),
                    {ok, {binary_to_list(Password), Attrs, {Balance, binary_to_list(Plan)}}}
            end;
        _ -> undefined
    end.
