-module(mod_preference).

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
	PTag = xml:get_subtag(SubEl, <<"preferences">>),
	#jid{luser = LUser, lserver = LServer} = _From,
	Username = ejabberd_odbc:escape(LUser),
	case Type of
		set ->
			IQ#iq{type = result, sub_el = [#xmlel{name = <<"comment">>, attrs = [], children = []}]};
		get ->
			?INFO_MSG("preferences ~n", []),
			Prefs = get_preferences,
			Result = [#xmlel{name = <<"preferences">>, attrs = [], children = Prefs}],
			IQ#iq{type = result, sub_el = Result}
	end.




get_preferences(LServer, PostId) ->
    case catch odbc_queries:get_preferences(LServer) of
      {selected, [<<"id">>, <<"preferences">>], Posts} ->
	  LItems = lists:map(fun ([N,P]) ->
				     #xmlel{name = <<"comment">>,
					    attrs = [{<<"id">>, N}, {<<"preference">>, P}],
					    children = []}
			     end,
			     Posts),
	  LItems;
      _ -> error
    end.


