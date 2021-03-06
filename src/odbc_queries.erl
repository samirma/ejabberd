%%%----------------------------------------------------------------------
%%% File    : odbc_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ODBC queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(odbc_queries).


-behaviour(ejabberd_config).

-author("mremond@process-one.net").

-export([get_db_type/0, update/5, update_t/4,
	 sql_transaction/2, get_last/2, set_last_t/4, del_last/2,
	 get_password/2, get_password_scram/2, set_password_t/3,
	 set_password_scram_t/6, add_user/3, add_user_scram/6,
	 del_user/2, del_user_return_password/3, list_users/1,
	 list_users/2, users_number/1, users_number/2,
	 add_spool_sql/2, add_spool/2, get_and_del_spool_msg_t/2,
	 del_spool_msg/2, get_roster/2, get_roster_jid_groups/2,
	 get_roster_groups/3, del_user_roster_t/2,
	 get_roster_by_jid/3, get_rostergroup_by_jid/3,
	 del_roster/3, del_roster_sql/2, update_roster/5,
	 update_roster_sql/4, roster_subscribe/4,
	 get_subscription/3, set_private_data/4,
	 set_private_data_sql/3, get_private_data/3,
	 get_private_data/2, del_user_private_storage/2,
	 get_default_privacy_list/2,
	 get_default_privacy_list_t/1, get_privacy_list_names/2,
	 get_privacy_list_names_t/1, get_privacy_list_id/3,
	 get_privacy_list_id_t/2, get_privacy_list_data/3,
	 get_privacy_list_data_by_id/2,
	 get_privacy_list_data_t/2,
	 get_privacy_list_data_by_id_t/1,
	 set_default_privacy_list/2,
	 unset_default_privacy_list/2, remove_privacy_list/2,
	 add_privacy_list/2, set_privacy_list/2,
	 del_privacy_lists/3, set_vcard/26, get_vcard/2,
	 escape/1, count_records_where/3, get_roster_version/2,
	 set_roster_version/2, opt_type/1,
	 update_view_post/2,
	 rate_post/4,
	 add_new_post/5, 
	 get_posts/4,
	 get_comments/2,
	 add_new_comment/6,
	 get_preferences_list/1,
	 hidder_from_user/4,
	 get_registration_id/4,
	 create_citiviti_user/3,
	 request_registration/4
		 
]).

-include("ejabberd.hrl").
-include("logger.hrl").

%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

get_db_type() -> generic.

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t([<<"update ">>, Table,
				    <<" set ">>, join(UPairs, <<", ">>),
				    <<" where ">>, Where, <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
		Res = ejabberd_odbc:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values ('">>, join(Vals, <<"', '">>),
				     <<"');">>]),
		case Res of
			{updated,1} -> ok;
			_ -> Res
		end
    end.

update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query(LServer,
				 [<<"update ">>, Table, <<" set ">>,
				  join(UPairs, <<", ">>), <<" where ">>, Where,
				  <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
		Res = ejabberd_odbc:sql_query(LServer,
				  [<<"insert into ">>, Table, <<"(">>,
				   join(Fields, <<", ">>), <<") values ('">>,
				   join(Vals, <<"', '">>), <<"');">>]),
		case Res of
			{updated,1} -> ok;
			_ -> Res
		end		   
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the ejabberd_odbc module to this one (odbc_queries)
sql_transaction(LServer, F) ->
    ejabberd_odbc:sql_transaction(LServer, F).

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select seconds, state from last where "
			       "username='">>,
			     Username, <<"'">>]).

set_last_t(LServer, Username, Seconds, State) ->
    update(LServer, <<"last">>,
	   [<<"username">>, <<"seconds">>, <<"state">>],
	   [Username, Seconds, State],
	   [<<"username='">>, Username, <<"'">>]).

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from last where username='">>, Username,
			     <<"'">>]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select password from users where username='">>,
			     Username, <<"';">>]).

get_password_scram(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select password, serverkey, salt, iterationcount from users where "
        "username='">>, Username, <<"';">>]).

set_password_t(LServer, Username, Pass) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"users">>,
						   [<<"username">>,
						    <<"password">>],
						   [Username, Pass],
						   [<<"username='">>, Username,
						    <<"'">>])
				  end).

set_password_scram_t(LServer, Username,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"users">>,
						   [<<"username">>,
						    <<"password">>,
						    <<"serverkey">>,
						    <<"salt">>,
						    <<"iterationcount">>],
						   [Username, StoredKey,
                                                    ServerKey, Salt,
                                                    IterationCount],
						   [<<"username='">>, Username,
						    <<"'">>])
				  end).

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"insert into users(username, password) "
			       "values ('">>,
			     Username, <<"', '">>, Pass, <<"');">>]).

add_user_scram(LServer, Username,
               StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"insert into users(username, password, serverkey, salt, iterationcount) "
			       "values ('">>,
			     Username, <<"', '">>, StoredKey, <<"', '">>,
                             ServerKey, <<"', '">>,
                             Salt, <<"', '">>,
                             IterationCount, <<"');">>]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from users where username='">>, Username,
			     <<"';">>]).

del_user_return_password(_LServer, Username, Pass) ->
    P =
	ejabberd_odbc:sql_query_t([<<"select password from users where username='">>,
				   Username, <<"';">>]),
    ejabberd_odbc:sql_query_t([<<"delete from users where username='">>,
			       Username, <<"' and password='">>, Pass,
			       <<"';">>]),
    P.

list_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username from users">>]).

list_users(LServer, [{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    list_users(LServer,
	       [{limit, End - Start + 1}, {offset, Start - 1}]);
list_users(LServer,
	   [{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    list_users(LServer,
	       [{prefix, Prefix}, {limit, End - Start + 1},
		{offset, Start - 1}]);
list_users(LServer, [{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select username from users " ++
                                     "order by username " ++
                                     "limit ~w offset ~w",
                                 [Limit, Offset]))]);
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select username from users " ++
                                     "where username like '~s%' " ++
                                     "order by username " ++
                                     "limit ~w offset ~w ",
                                 [Prefix, Limit, Offset]))]).

users_number(LServer) ->
    Type = ejabberd_config:get_option({odbc_type, LServer},
                                      fun(pgsql) -> pgsql;
                                         (mysql) -> mysql;
                                         (sqlite) -> sqlite;
                                         (odbc) -> odbc
                                      end, odbc),
    case Type of
      pgsql ->
	  case
	    ejabberd_config:get_option(
              {pgsql_users_number_estimate, LServer},
              fun(V) when is_boolean(V) -> V end,
              false)
	      of
	    true ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select reltuples from pg_class where "
                                           "oid = 'users'::regclass::oid">>]);
	    _ ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select count(*) from users">>])
	  end;
      _ ->
	  ejabberd_odbc:sql_query(LServer,
				  [<<"select count(*) from users">>])
    end.

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:fwrite(
                                 "select count(*) from users " ++
                                     %% Warning: Escape prefix at higher level to prevent SQL
                                     %%          injection.
                                     "where username like '~s%'",
                                 [Prefix]))]);
users_number(LServer, []) ->
    users_number(LServer).

add_spool_sql(Username, XML) ->
    [<<"insert into spool(username, xml) values ('">>,
     Username, <<"', '">>, XML, <<"');">>].







%%%%%%% Posts



add_new_post(LServer, Username, XML, LongitudeAttr, LatituteAttr) ->
    ejabberd_odbc:sql_query(LServer,[<<"insert into posts(username, post, localization) "
				 "values ('">>, Username, <<"', '">>, XML, <<"', ST_MakePoint(">>, LongitudeAttr, <<", ">>, LatituteAttr, <<"));">>]).


get_posts(LServer, LatituteAttr, LongitudeAttr, Range) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"SELECT id, username, post, rate, rates_count, views_count FROM posts WHERE " 
   "st_distance_sphere(ST_MakePoint(">>, LongitudeAttr, <<", ">>, LatituteAttr, <<"), localization) < ">>, Range, <<"  ORDER BY created_at DESC;">>]).

update_view_post(LServer, PostId) ->
    ejabberd_odbc:sql_query(LServer, [<<"UPDATE posts set views_count = views_count + 1 WHERE id=">>,  PostId, <<";">>]).


rate_post(LServer, Username, PostId, Rate) ->
	ejabberd_odbc:sql_query(LServer, [<<"DELETE FROM rates posts WHERE post_id=">>,  PostId, <<" AND username='">>, Username, <<"';">>]),
	Value = get_rate_value(Rate),
	insert_rate(LServer, Username, PostId, Value),
    ejabberd_odbc:sql_query(LServer, [<<"UPDATE posts set rates_count = (select count(id) FROM rates WHERE post_id=">>,  PostId, <<" ), rate = (select SUM(rate) FROM rates WHERE post_id=">>,  PostId, <<" ) WHERE id=">>,  PostId, <<";">>]).

insert_rate(LServer, Username, PostId, Value) ->
	?INFO_MSG("Rate value ~p  ~p ~n", [PostId, Value]),
	ejabberd_odbc:sql_query(LServer, [<<"insert into public.rates (post_id, username, rate) VALUES (">>,  PostId, <<", '">>, Username, <<"', ">>,  Value, <<") ;">>]).
	
get_rate_value(Rate) ->
    Code = if
        Rate =:= <<"up">> -> <<"1">>;
        Rate =:= <<"down">> -> <<"-1">>;
        true -> unknown
    end.

%%%%%%% Comments

add_new_comment(LServer, Username, PostId, Comment, LatituteAttr, LongitudeAttr) ->
    ejabberd_odbc:sql_query(LServer,[<<"insert into comments(username, post_id, commentary, localization) "
				 "values ('">>, Username, <<"', ">>, PostId, <<", '">>, Comment,<<"', ST_MakePoint(">>, LongitudeAttr, <<", ">>, LongitudeAttr, <<"));">>]).

get_comments(LServer, PostId) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"SELECT id, commentary, rate FROM comments where post_id=">>,  PostId, <<" ORDER BY created_at DESC;">>]).


%%%%%% Register

create_citiviti_user(LServer, Username, IdPhone) ->
    ejabberd_odbc:sql_query(LServer,[<<"insert into registed_users(username, phone_id) values ('">>, Username, <<"', ">>, IdPhone, <<");">>]).

request_registration(LServer, Username, Phone, Code) -> 
    ejabberd_odbc:sql_query(LServer,[<<"insert into user_phone(username, phone, register_code) values ('">>, Username, <<"', '">>, Phone, <<"', '">>, Code, <<"');">>]).

get_registration_id(LServer, Username, Phone, Code) -> 
	Query = [<<"SELECT id FROM user_phone WHERE username = '">>, Username, <<"' AND phone = '">>, Phone, <<"' AND register_code = '">>, Code, <<"';">>],
	?INFO_MSG("Query ~p ~n", [Query]),
    ejabberd_odbc:sql_query(LServer,Query).


%%%%%%% Preferences

set_preference(LServer, Username, PostId, Comment, LatituteAttr, LongitudeAttr) ->
    ejabberd_odbc:sql_query(LServer,[<<"insert into comments(username, post_id, commentary, localization) "
				 "values ('">>, Username, <<"', ">>, PostId, <<", '">>, Comment,<<"', ST_MakePoint(">>, LongitudeAttr, <<", ">>, LongitudeAttr, <<"));">>]).

get_preferences_list(LServer) ->
	?INFO_MSG("get_preferences_list", []),
    ejabberd_odbc:sql_query(LServer,
			    [<<"SELECT id, preference FROM preferences ORDER BY preference;">>]).


%%%%%%% Hidder Post

hidder_from_user(LServer, Username, Id, Type) ->

	case Type of
		post ->
			Entity = "hidden_posts";
		comment ->
			Entity = "hidden_comments"
	end,

    ejabberd_odbc:sql_query(LServer,[<<"insert into ">>, Entity, <<"(hidden_from_user, hidden_id ) "
				 "values ('">>, Username, <<"', ">>, Id, <<");">>]).

%%%%%%% 



add_spool(LServer, Queries) ->
    ejabberd_odbc:sql_transaction(LServer, Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    F = fun () ->
		Result =
		    ejabberd_odbc:sql_query_t([<<"select username, xml from spool where "
						 "username='">>,
					       Username,
					       <<"'  order by seq;">>]),
		ejabberd_odbc:sql_query_t([<<"delete from spool where username='">>,
					   Username, <<"';">>]),
		Result
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from spool where username='">>, Username,
			     <<"';">>]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username, jid, nick, subscription, "
			       "ask, askmessage, server, subscribe, "
			       "type from rosterusers where username='">>,
			     Username, <<"'">>]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select jid, grp from rostergroups where "
			       "username='">>,
			     Username, <<"'">>]).

get_roster_groups(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select grp from rostergroups where username='">>,
			       Username, <<"' and jid='">>, SJID, <<"';">>]).

del_user_roster_t(LServer, Username) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  ejabberd_odbc:sql_query_t([<<"delete from rosterusers       where "
								       "username='">>,
								     Username,
								     <<"';">>]),
					  ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
								       "username='">>,
								     Username,
								     <<"';">>])
				  end).

get_roster_by_jid(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select username, jid, nick, subscription, "
				 "ask, askmessage, server, subscribe, "
				 "type from rosterusers where username='">>,
			       Username, <<"' and jid='">>, SJID, <<"';">>]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select grp from rostergroups where username='">>,
			     Username, <<"' and jid='">>, SJID, <<"'">>]).

del_roster(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"delete from rosterusers       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]).

del_roster_sql(Username, SJID) ->
    [[<<"delete from rosterusers       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"delete from rostergroups       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>]].

update_roster(_LServer, Username, SJID, ItemVals,
	      ItemGroups) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID,
	      <<"'">>]),
    ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    lists:foreach(fun (ItemGroup) ->
			  ejabberd_odbc:sql_query_t([<<"insert into rostergroups(           "
						       "   username, jid, grp)  values ('">>,
						     join(ItemGroup,
							  <<"', '">>),
						     <<"');">>])
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals,
		  ItemGroups) ->
    [[<<"delete from rosterusers       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"insert into rosterusers(            "
	"  username, jid, nick,              "
	" subscription, ask, askmessage,     "
	"          server, subscribe, type)  "
	"values ('">>,
      join(ItemVals, <<"', '">>), <<"');">>],
     [<<"delete from rostergroups       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>]]
      ++
      [[<<"insert into rostergroups(           "
	  "   username, jid, grp)  values ('">>,
	join(ItemGroup, <<"', '">>), <<"');">>]
       || ItemGroup <- ItemGroups].

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID,
	      <<"'">>]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select subscription from rosterusers "
			       "where username='">>,
			     Username, <<"' and jid='">>, SJID, <<"'">>]).

set_private_data(_LServer, Username, LXMLNS, SData) ->
    update_t(<<"private_storage">>,
	     [<<"username">>, <<"namespace">>, <<"data">>],
	     [Username, LXMLNS, SData],
	     [<<"username='">>, Username, <<"' and namespace='">>,
	      LXMLNS, <<"'">>]).

set_private_data_sql(Username, LXMLNS, SData) ->
    [[<<"delete from private_storage where username='">>,
      Username, <<"' and namespace='">>, LXMLNS, <<"';">>],
     [<<"insert into private_storage(username, "
	"namespace, data) values ('">>,
      Username, <<"', '">>, LXMLNS, <<"', '">>, SData,
      <<"');">>]].

get_private_data(LServer, Username, LXMLNS) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select data from private_storage where "
			       "username='">>,
			     Username, <<"' and namespace='">>, LXMLNS,
			     <<"';">>]).

get_private_data(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
                            [<<"select namespace, data from private_storage "
                               "where username='">>, Username, <<"';">>]).

del_user_private_storage(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from private_storage where username='">>,
			     Username, <<"';">>]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN,
	  SFamily, SGiven, SLBDay, SLCTRY, SLEMail, SLFN,
	  SLFamily, SLGiven, SLLocality, SLMiddle, SLNickname,
	  SLOrgName, SLOrgUnit, SLocality, SMiddle, SNickname,
	  SOrgName, SOrgUnit, SVCARD, Username) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"vcard">>,
						   [<<"username">>,
						    <<"vcard">>],
						   [LUsername, SVCARD],
						   [<<"username='">>, LUsername,
						    <<"'">>]),
					  update_t(<<"vcard_search">>,
						   [<<"username">>,
						    <<"lusername">>, <<"fn">>,
						    <<"lfn">>, <<"family">>,
						    <<"lfamily">>, <<"given">>,
						    <<"lgiven">>, <<"middle">>,
						    <<"lmiddle">>,
						    <<"nickname">>,
						    <<"lnickname">>, <<"bday">>,
						    <<"lbday">>, <<"ctry">>,
						    <<"lctry">>, <<"locality">>,
						    <<"llocality">>,
						    <<"email">>, <<"lemail">>,
						    <<"orgname">>,
						    <<"lorgname">>,
						    <<"orgunit">>,
						    <<"lorgunit">>],
						   [Username, LUsername, SFN,
						    SLFN, SFamily, SLFamily,
						    SGiven, SLGiven, SMiddle,
						    SLMiddle, SNickname,
						    SLNickname, SBDay, SLBDay,
						    SCTRY, SLCTRY, SLocality,
						    SLLocality, SEMail, SLEMail,
						    SOrgName, SLOrgName,
						    SOrgUnit, SLOrgUnit],
						   [<<"lusername='">>,
						    LUsername, <<"'">>])
				  end).

get_vcard(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select vcard from vcard where username='">>,
			     Username, <<"';">>]).

get_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from privacy_default_list "
			       "where username='">>,
			     Username, <<"';">>]).

get_default_privacy_list_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"select name from privacy_default_list "
				 "where username='">>,
			       Username, <<"';">>]).

get_privacy_list_names(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from privacy_list where "
			       "username='">>,
			     Username, <<"';">>]).

get_privacy_list_names_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"select name from privacy_list where "
				 "username='">>,
			       Username, <<"';">>]).

get_privacy_list_id(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select id from privacy_list where username='">>,
			     Username, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_id_t(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"select id from privacy_list where username='">>,
			       Username, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_data(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from privacy_list_data "
			       "where id = (select id from privacy_list "
			       "where             username='">>,
			     Username, <<"' and name='">>, SName,
			     <<"') order by ord;">>]).

get_privacy_list_data_t(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
                                 "match_iq, match_message, match_presence_in, "
                                 "match_presence_out from privacy_list_data "
                                 "where id = (select id from privacy_list "
                                 "where             username='">>,
                               Username, <<"' and name='">>, SName,
                               <<"') order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from privacy_list_data "
			       "where id='">>,
			     ID, <<"' order by ord;">>]).

get_privacy_list_data_by_id_t(ID) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
				 "match_iq, match_message, match_presence_in, "
				 "match_presence_out from privacy_list_data "
				 "where id='">>,
			       ID, <<"' order by ord;">>]).

set_default_privacy_list(Username, SName) ->
    update_t(<<"privacy_default_list">>,
	     [<<"username">>, <<"name">>], [Username, SName],
	     [<<"username='">>, Username, <<"'">>]).

unset_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_default_list    "
			       "   where username='">>,
			     Username, <<"';">>]).

remove_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"delete from privacy_list where username='">>,
			       Username, <<"' and name='">>, SName, <<"';">>]).

add_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"insert into privacy_list(username, name) "
				 "values ('">>,
			       Username, <<"', '">>, SName, <<"');">>]).



set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t([<<"delete from privacy_list_data where "
				 "id='">>,
			       ID, <<"';">>]),
    lists:foreach(fun (Items) ->
			  ejabberd_odbc:sql_query_t([<<"insert into privacy_list_data(id, t, "
						       "value, action, ord, match_all, match_iq, "
						       "match_message, match_presence_in, match_prese"
						       "nce_out ) values ('">>,
						     ID, <<"', '">>,
						     join(Items, <<"', '">>),
						     <<"');">>])
		  end,
		  RItems).

del_privacy_lists(LServer, Server, Username) ->
%% Characters to escape
%% Count number of records in a table given a where clause
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_list where username='">>,
			     Username, <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_list_data where "
			       "value='">>,
			     <<Username/binary, "@", Server/binary>>,
			     <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_default_list where "
			       "username='">>,
			     Username, <<"';">>]).

escape($\000) -> <<"\\0">>;
escape($\n) -> <<"\\n">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(C) -> <<C>>.

count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select count(*) from ">>, Table, <<" ">>,
			     WhereClause, <<";">>]).

get_roster_version(LServer, LUser) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select version from roster_version where "
			       "username = '">>,
			     LUser, <<"'">>]).

set_roster_version(LUser, Version) ->
    update_t(<<"roster_version">>,
	     [<<"username">>, <<"version">>], [LUser, Version],
	     [<<"username = '">>, LUser, <<"'">>]).

opt_type(odbc_type) ->
    fun (pgsql) -> pgsql;
	(mysql) -> mysql;
	(sqlite) -> sqlite;
	(mssql) -> mssql;
	(odbc) -> odbc
    end;
opt_type(pgsql_users_number_estimate) ->
    fun (V) when is_boolean(V) -> V end;
opt_type(_) -> [odbc_type, pgsql_users_number_estimate].






