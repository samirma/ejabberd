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
    LatituteAttr = xml:get_tag_attr_s(<<"latitute">>, PTag), 
    LongitudeAttr = xml:get_tag_attr_s(<<"longitude">>, PTag),
    case Type of
      set ->
    	  ?INFO_MSG("### Post incomming ~p on ~p ~p~n", [PostText, LatituteAttr, LongitudeAttr]),
    	  #jid{luser = LUser, lserver = LServer} = _From,
    	  Username = ejabberd_odbc:escape(LUser),
    	  odbc_queries:add_new_post(To#jid.lserver, Username, PostText, LatituteAttr, LongitudeAttr),
	  IQ#iq{type = result, sub_el = [#xmlel{name = <<"post">>, attrs = [], children = []}]};
      get ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.



mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(show_os) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [iqdisc, show_os].
