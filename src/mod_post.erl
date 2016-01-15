-module(mod_post).

-author('santonio@citiviti.com').

-protocol({xep, 92, '1.1'}).

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3,
		 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
	?INFO_MSG("Post module online", []),
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
							 one_queue),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host,
								  ?NS_POST, ?MODULE, process_local_iq,
								  IQDisc).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
									 ?NS_POST).

process_local_iq(_From, To,
				 #iq{id = _ID, type = Type, xmlns = _XMLNS,
					 sub_el = SubEl} =
					 IQ) ->
	PTag = xml:get_subtag(SubEl, <<"post">>),
	PostText = xml:get_tag_cdata(PTag),
	Lat = xml:get_tag_attr_s(<<"latitude">>, PTag), 
	Long = xml:get_tag_attr_s(<<"longitude">>, PTag),
	Rate = xml:get_tag_attr_s(<<"rate">>, PTag),
	#jid{luser = LUser, lserver = LServer} = _From,
	Username = ejabberd_odbc:escape(LUser),
	case Type of
		set ->
			?INFO_MSG("Post incomming ~p on Long ~p Lat ~p~n", [PostText, Long, Lat]),
			odbc_queries:add_new_post(To#jid.lserver, Username, PostText, Long, Lat),
			ResultQuery = [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_POST}],
			    children = []}],
			IQ#iq{type = result, sub_el = ResultQuery};
		get ->
			Range = xml:get_tag_attr_s(<<"within">>, PTag),
			Posts = process_posts_get(To#jid.lserver, Lat, Long, Range),
			Result = [#xmlel{name = <<"posts">>, attrs = [{<<"xmlns">>, ?NS_POST}], children = Posts}],
			ResultQuery = [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_POST}],
			    children = Result}],
			IQ#iq{type = result, sub_el = ResultQuery}
	end.




process_posts_get(LServer, LatituteAttr, LongitudeAttr, Range) ->
    case catch odbc_queries:get_posts(LServer, LatituteAttr, LongitudeAttr, Range) of
      {selected, [<<"id">>, <<"username">>, <<"post">>, <<"rate">>, <<"rates_count">>, <<"views_count">>], Posts} ->
	  LItems = lists:map(fun ([N, U, P, R, H, V]) ->
				     #xmlel{name = <<"post">>,
					    attrs = [{<<"id">>, N}, {<<"user">>, U}, {<<"rate">>, R}, {<<"rates_count">>, H}, {<<"views">>, V}],
					    children = [{xmlcdata, P}]}
			     end,
			     Posts),
	  LItems;
      _ -> error
    end.




mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(show_os) ->
	fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [iqdisc, show_os].
