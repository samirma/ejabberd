-module(mod_ctreg).

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
								  ?NS_CITIVITI_REGISTER, ?MODULE, process_local_iq,
								  IQDisc).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
									 ?NS_CITIVITI_REGISTER).

process_local_iq(_From, To,
				 #iq{id = _ID, type = Type, xmlns = _XMLNS,
					 sub_el = SubEl} =
					 IQ) ->
	PTag = xml:get_subtag(SubEl, <<"register">>),
	PhoneNumber = xml:get_tag_attr_s(<<"request_number">>, PTag), 
	#jid{luser = LUser, lserver = LServer} = _From,
	Username = ejabberd_odbc:escape(LUser),
	?INFO_MSG("Request register incomming from ~p to phone ~p ~n", [Username, PhoneNumber]),
	LServer = To#jid.lserver,
	case Type of
		set ->
			Code = xml:get_tag_attr_s(<<"code">>, PTag), 
			Result = mod_citiviti_register:register_user(LServer, Username, PhoneNumber, Code),
			IQ#iq{type = result, sub_el = []};
		get ->
			Result = mod_citiviti_register:request_registration(LServer, Username, PhoneNumber),
			IQ#iq{type = result, sub_el = []}
	end.


