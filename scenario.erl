%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/10/30
%%% -------------------------------------------------------------------
-module(scenario).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-compile(export_all).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------


%% ====================================================================
%% External functions
%% ====================================================================


start() ->
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> scenario_gui:start(SelfPid) end),
	put(chgui, Pid),
	event_loop(off,off),
	true.



%-------------------------------------
%% process state
%%-------------------------------------
%
event_loop(Mode,LogMod) ->
	receive
                %%-------------------------------------------
		%% scenario
                %%-------------------------------------------
		{scsave, chgui,Parameters} when Mode == off ->
			Scname = getParam(scname,Parameters),
			put(scname,Scname),
			put(sc,[]),
			{ok,TRef} = timer:send_interval(calets:getSavTime(),{sctimer, timer,[]}),
			put(cresc,TRef),
			event_loop(on,LogMod);

		{sctimer,timer,_} when Mode == on ->
			gen_server:cast({global,output},{getinfo_req, scenario,[speed,0]}),
			event_loop(Mode,LogMod);

		{getinfo_res,output,Parameters}when Mode == on ->
			Speed = getParam(speed,Parameters),
			put(sc,[[Speed] | get(sc)]),
			event_loop(Mode,LogMod);

		{scstop, chgui,_Parameters}  when Mode == on ->
			timer:cancel(get(cresc)),
			case get(sc) of
				undef ->
					creSc(get(scname),[]);
				CreSc -> 
					Scr = lists:reverse(CreSc),
					creSc(get(scname),Scr)
			end,
			event_loop(off,LogMod);

		{scexe,chgui,Parameters} when Mode =:= off -> %% scenario execute
			Scname = getParam(scname,Parameters),
			Sc = getSc(Scname),
			gen_server:cast({global,frsensor},{scenario_req, scenario,[scenario,Sc]}),
			event_loop(exe,LogMod);

		{scexestop,chgui,_} -> %% stop scenario execute
			gen_server:cast({global,frsensor},{scenario_can, scenario,[]}),
			event_loop(off,LogMod);

		{scenario_res, frsensor,_} ->     %% finish execute 
			event_loop(off,LogMod);

                %%-------------------------------------------
		%%Log
                %%-------------------------------------------
		{logstart,chgui, Parameters} when LogMod =:= off ->
			Logname = getParam(logname,Parameters),
			putloginit(Logname),
			gen_server:cast({global,frsensor},{logstart_ind, scenario,[]}),
			event_loop(Mode,start);

		{log_inf, frsensor,Parameters} when LogMod =:= start->     %% loginfo
			Speed = getParam(speed,Parameters),
			FDis  = getParam(distance,Parameters),
			putlog(Speed,FDis),
			event_loop(Mode,LogMod);

		{logstop,chgui, _} ->
			gen_server:cast({global,frsensor},{logstop_ind, scenario,[]}),
			file:close(get(logf)),
			event_loop(Mode,off);
                %%-------------------------------------------
		%% other
                %%-------------------------------------------

		{'EXIT', _Pid, _Why} ->
%			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
%			exit;


		_Any ->
%%			io:format("scenario unknown type received : ~p,~p,~n", [_Any,Mode])
			event_loop(Mode,LogMod)

	end.

%-------------------------------------
%% tool
%%-------------------------------------
%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

string_to_intList(In,Out)->
	case string:to_integer(In) of
		{error,_}->
			case In of
				[_T|B] -> 
					string_to_intList(B,Out);
				_ -> 
					lists:reverse(Out)
			end;		
		{Int,B}->
			string_to_intList(B,[Int|Out])
	end.


list_to_Csv([T|[]],Out) when is_atom(T)->
	Ret = [erlang:atom_to_list(T)| Out],
	lists:reverse(Ret);

list_to_Csv([T|[]],Out) when is_integer(T)->
	Ret = [erlang:integer_to_list(T)| Out],
	lists:reverse(Ret);

list_to_Csv([T|[]],Out) when is_float(T)->
	Ret = [erlang:float_to_list(T)| Out],
	lists:reverse(Ret);

list_to_Csv([T|[]],Out) ->
	Ret = [T| Out],
	lists:reverse(Ret);

list_to_Csv([T|B],Out) when is_atom(T) ->
	list_to_Csv(B,[",", erlang:atom_to_list(T) | Out]);

list_to_Csv([T|B],Out) when is_integer(T) ->
	list_to_Csv(B,[",", erlang:integer_to_list(T) | Out]);

list_to_Csv([T|B],Out) when is_float(T) ->
	list_to_Csv(B,[",", erlang:float_to_list(T) | Out]);

list_to_Csv([T|B],Out) ->
	list_to_Csv(B,[",",T | Out]).


creSc(Scfilename,Sc)->
	{ok,S} = file:open(Scfilename,write),
	lists:foreach(fun(X) -> io:format(S,"~s~n",[list_to_Csv(X,[])]) end,Sc),
	file:close(S).

getSc(Scfilename)->
	try file:open(Scfilename,read) of
		{ok,S} ->
			Ret = readLine(S,[]),
			file:close(S),
			Ret;
		Any->
			erlang:display(Any)
	catch
		Cat ->
			erlang:display("error"++Cat)

	end.

readLine(OpenF,Out)->
	case io:get_line(OpenF,'') of
		eof ->
			lists:reverse(Out);
		Line->
			ILine = string_to_intList(Line,[]),
			readLine(OpenF,[ILine| Out])
	end.

putloginit(LogFname)->
	{ok,S} = file:open(LogFname,write),
	put(logf,S).

putlog(Speed,FDis) ->
	io:format(get(logf),"~s~n",[list_to_Csv([Speed,FDis],[])]).


