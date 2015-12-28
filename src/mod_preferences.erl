-module(mod_preferences).

-author('santonio@citiviti.com').

-protocol({xep, 92, '1.1'}).

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
							 one_queue),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host,
								  ?NS_CITIVITI_PREFERENCE, ?MODULE, process_local_iq,
								  IQDisc).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
									 ?NS_CITIVITI_PREFERENCE).

process_local_iq(_From, To,
				 #iq{id = _ID, type = Type, xmlns = _XMLNS,
					 sub_el = SubEl} =
					 IQ) ->

	#jid{luser = LUser, lserver = LServer} = _From,
	?INFO_MSG("Preferences ~n", []),
	case Type of
		set ->
			IQ#iq{type = set_result, sub_el = []};
		get ->
			?INFO_MSG("Getting preferences ~n", []),
			Prefs = process_get(To#jid.lserver),
			Xmls = [#xmlel{name = <<"prefererences">>, attrs = [], children = Prefs}],
			IQ#iq{type = result, sub_el = Xmls}
	end.




process_get(LServer) ->
	?INFO_MSG("Teste!!! ~p ~n", [odbc_queries:get_preferences_list(LServer)]),
	case catch odbc_queries:get_preferences_list(LServer) of
      {selected,[<<"id">>,<<"preference">>], Xmls} ->
	  LItems = lists:map(fun ([Id,Pref]) ->
				     #xmlel{name = <<"preference">>,
					    attrs = [{<<"id">>, Id}],
					    children = [{xmlcdata, Pref}]}
			     end,
			     Xmls),
	  LItems;
      _ -> error
    end.