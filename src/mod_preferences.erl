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
	PTag = xml:get_subtag(SubEl, <<"rate">>),
	PostId = xml:get_tag_attr_s(<<"post_id">>, PTag), 
	TypeRate = xml:get_tag_attr_s(<<"type">>, PTag), 
	#jid{luser = LUser, lserver = LServer} = _From,
	Username = ejabberd_odbc:escape(LUser),
	case Type of
		set ->
			?INFO_MSG("Rate incomming ~n", []),
			IQ#iq{type = result, sub_el = []};
		get ->
			Prefs = get_preferences(To#jid.lserver),
			Result = [#xmlel{name = <<"prefererences">>, attrs = [], children = Prefs}],
			IQ#iq{type = result, sub_el = Result}
	end.




get_preferences(LServer) ->
    case catch ejabberd_odbc:get_preferences(LServer) of
      {selected, [<<"id">>, <<"username">>, <<"post">>, <<"rate">>, <<"rates_count">>, <<"views_count">>], Posts} ->
	  LItems = lists:map(fun ([N,U,P, R, H, V]) ->
				     #xmlel{name = <<"post">>,
					    attrs = [{<<"id">>, N}, {<<"user">>, U}, {<<"rate">>, R}, {<<"rates_count">>, R}, {<<"views">>, V}],
					    children = [{xmlcdata, P}]}
			     end,
			     Posts),
	  LItems;
      _ -> error
    end.