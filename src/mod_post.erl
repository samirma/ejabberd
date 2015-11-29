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
    ?INFO_MSG("Post incomming", []),
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  ?INFO_MSG("Posted!!!!", []);
    end.

get_os() ->
    {Osfamily, Osname} = os:type(),
    OSType = list_to_binary([atom_to_list(Osfamily), $/, atom_to_list(Osname)]),
    OSVersion = case os:version() of
		  {Major, Minor, Release} ->
		      iolist_to_binary(io_lib:format("~w.~w.~w",
						     [Major, Minor, Release]));
		  VersionString -> VersionString
		end,
    OS = <<OSType/binary, " ", OSVersion/binary>>,
    #xmlel{name = <<"os">>, attrs = [],
	   children = [{xmlcdata, OS}]}.

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(show_os) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [iqdisc, show_os].
