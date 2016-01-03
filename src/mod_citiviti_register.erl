-module(mod_citiviti_register).

-author('santonio@citiviti.com').

-protocol({xep, 92, '1.1'}).

-export([request_registration/3 
		,register_user/4
	]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").


request_registration(LServer, Username, PhoneNumber)  ->
	Code = <<"4">>,
	odbc_queries:request_registration(LServer, Username, PhoneNumber, Code).


register_user(LServer, Username, PhoneNumber, Code)  ->
	?INFO_MSG("Request register incomming from ~p to phone ~p  ~p ~n", [Username, PhoneNumber, Code]),
	Lists = get_registration_id(LServer, Username, PhoneNumber, Code),
	?INFO_MSG("Attempt to register ~p ~n", [Lists]).



get_registration_id(LServer, Username, PhoneNumber, Code)  ->
	QueryResult = odbc_queries:get_registration_id(LServer, Username, PhoneNumber, Code),
	?INFO_MSG("QueryResult ~p ~n", [QueryResult]),
	case catch QueryResult of
      {selected,[<<"id">>], Xmls} ->
	  LItems = lists:map(fun ([Id]) ->
					 ?INFO_MSG("Id found ~n", []),
				     odbc_queries:create_citiviti_user(LServer, Username, Id)
			     end,
			     Xmls),
	  LItems;
      _ -> error
    end.