%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(wheel_gui).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(GPRAPH_WIDE, 200).
-define(RPM_MAX, 2500).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(Parent, Position) ->
	GSHandle = gs:start(),
	{WinX, WinY} = get_default_win_position(Position),
	WinHandle = gs:create(window, GSHandle,
						  [
						   {title, string:concat("Wheel : ", atom_to_list(Position))},
						   {width, 250},
						   {height,210},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),
	Canvas = gs:create(canvas, WinHandle, [
									   {x, 10},
									   {y, 0},
									   {width, 40},
									   {height, 70},
									   {bg, black}]),
	gs:create(polygon, Canvas, [{coords, [{0, 10}, {10, 7}, {30, 7}, {40, 10}]}, {fg, green}, {fill, green}]),
	gs:create(rectangle, Canvas, [{coords, [{0, 10}, {40, 70}]}, {fg, green}, {fill, green}]),
	gs:create(rectangle, Canvas, [
							  {coords, [{5, 15}, {15, 30}]},
							  {bw, 1}, get_fill_option(Position, wheelFleft)]),
	gs:create(rectangle, Canvas, [
							  {coords, [{25, 15}, {35, 30}]},
							  {bw, 1}, get_fill_option(Position, wheelFright)]),
	gs:create(rectangle, Canvas, [
							  {coords, [{5, 50}, {15, 65}]},
							  {bw, 1}, get_fill_option(Position, wheelRleft)]),
	gs:create(rectangle, Canvas, [
							  {coords, [{25, 50}, {35, 65}]},
							  {bw, 1}, get_fill_option(Position, wheelRright)]),
	put(rpmview,gs:create(label, WinHandle, [{x, 55}, {y, 24}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 25}, {width, 100}, {label, {text, "0 [rpm]"}}, {font, {screen,bold, 12}}])),
	gs:create(label, WinHandle, [{x, 5}, {y, 80}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 200}, {label, {text, "rpm Graph(per 0.4 second) <-"}}, {font, {screen,bold, 10}}]),
	gs:create(label, WinHandle, [{x, 215}, {y, 185}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 30}, {label, {text, "0 "}}, {font, {screen,bold, 10}}]),
	gs:create(label, WinHandle, [{x, 215}, {y, 95}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 30}, {label, {text, integer_to_list(?RPM_MAX)}}, {font, {screen,bold, 9}}]),
	Canvas2 = gs:create(canvas, WinHandle, [
									   {x, 10},	
									   {y, 95},
									   {width, ?GPRAPH_WIDE},
									   {height,100},
									   {bg, green}]),
	ViewList = crelist(0,[]),
	put(graph,gs:create(line,Canvas2,[{coords,ViewList}, {arrow,last},{width,2},{fg,red}])),


	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text, "quit"}} ,{width,80},{height,25}, {x,160},{y,24}])),
	put(parentPid, Parent),
	put(position, Position),
	gs:config(WinHandle, {map, true}),
	event_loop(0,ViewList),
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
crelist(Count,List) when Count < ?GPRAPH_WIDE->
		crelist(Count+1,[getgraphval({Count, 0}) | List]);
crelist(_,List) ->
		List.

getgraphval({Cnt,Val}) when Val >?RPM_MAX -> {Cnt,0};
getgraphval({Cnt,Val}) -> {Cnt,100 - round(Val / ?RPM_MAX*100)}.

modlist(New,List)-> 
		List2 = modlist(lists:reverse(List)),
		[{Cnt,_}| Tail] = lists:reverse(List2),
		[getgraphval({Cnt,New}) | Tail].

modlist([{Cnt1,_},{Cnt2,Val2}] )->
	[{Cnt1,Val2},{Cnt2,Val2}]; 
modlist([{Cnt1,_},{Cnt2,Val2} | T] )->
	[{Cnt1,Val2} | modlist([{Cnt2,Val2}| T]) ].

review(Rpm,RpmList)->
	RpmList2 = modlist(Rpm,RpmList),
	gs:config(get(rpmview),{label, {text, integer_to_list(Rpm) ++ "[rpm]"}}),
	gs:config(get(graph), {coords, RpmList2}),
	RpmList2.

get_default_win_position(Position) ->
	case Position of
		wheelFleft ->
			{30, 30};
		wheelFright ->
			{500, 30};
		wheelRleft ->
			{30, 530};
		wheelRright ->
			{500, 530}
	end.

get_fill_option(Position, TargetPosition) ->
	if
		Position == TargetPosition ->
			{fill, red};
		true ->
			{fill, none}
	end.


event_loop(Rpm,RpmList) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;
		{state_inf,parwheel,slipstart}->
			gs:config(get(mainWindow),{bg, yellow}),
			event_loop(Rpm,RpmList);
		{state_inf,parwheel,slipstop}->
			gs:config(get(mainWindow),{bg, black}),
			event_loop(Rpm,RpmList);
		{state_inf,parwheel, Newrpm} ->
		%%	io:format("wheel_gui state_inf received from parwheel Para=~p ~n", [Newrpm]),
			RpmList2 = review(Newrpm,RpmList),
			event_loop(Newrpm,RpmList2);
		_ ->
			event_loop(Rpm,RpmList)
%	after 200 ->
%		RpmList2 = review(Rpm,RpmList),
%		event_loop(Rpm,RpmList2)
	end.
