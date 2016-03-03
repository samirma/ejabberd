-module(mod_rate).

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
								  ?NS_RATE, ?MODULE, process_local_iq,
								  IQDisc).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
									 ?NS_RATE).

process_local_iq(_From, To,
				 #iq{id = _ID, type = Type, xmlns = _XMLNS,
					 sub_el = SubEl} =
					 IQ) ->
	PTag = xml:get_subtag(SubEl, <<"post">>),
	PostId = xml:get_tag_attr_s(<<"post_id">>, PTag), 
	Rate = xml:get_tag_attr_s(<<"rate">>, PTag),
	#jid{luser = LUser, lserver = LServer} = _From,
	Username = ejabberd_odbc:escape(LUser),
	case Type of
		set ->
			?INFO_MSG("Rating Post ~p ~p ~n", [PostId, Rate]),
			odbc_queries:rate_post(To#jid.lserver, Username, PostId, Rate),
			IQ#iq{type = result, sub_el = [#xmlel{name = <<"post">>, attrs = [{<<"xmlns">>, ?NS_RATE}], children = []}]};
		get ->
			IQ#iq{type = error, sub_el = []}
	end.


