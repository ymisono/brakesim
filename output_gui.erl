%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(output_gui).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(GPRAPH_WIDE, 70).
-define(GPRAPH_WIDE2, 200).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(Parent) ->
	GSHandle = gs:start(),
	{WinX, WinY} = {800, 265},
	WinHandle = gs:create(window, GSHandle,
						  [
						   {title, "OUTPUT"},
						   {width, 330},
						   {height,210},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),

	Canvas = gs:create(canvas, WinHandle, [{x, 10},{y, 0},{width, 70},{height, 100},{bg, black}]),

	gs:create(rectangle, Canvas, [{coords, [{0, 10}, {80, 90}]}, {fg, green}, {fill, green}]),

	gs:create(rectangle, Canvas, [{coords, [{5, 15}, {30, 45}]}, {bw, 1}, {fill, red} ]),
	put(nw,gs:create(label, Canvas, [{x, 10}, {y, 24}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 12}, {width, 15}, {label, {text, "25"}}, {font, {screen,bold, 12}}])),

	gs:create(rectangle, Canvas, [{coords, [{35, 15}, {60, 45}]},{bw, 1}, {fill, red}]),
	put(ne,gs:create(label, Canvas, [{x, 40}, {y, 24}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 12}, {width, 15}, {label, {text, "25"}}, {font, {screen,bold, 12}}])),

	gs:create(rectangle, Canvas, [{coords, [{5, 50}, {30, 80}]}, {bw, 1}, {fill, red}]),
	put(sw,gs:create(label, Canvas, [{x, 10}, {y, 59}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 12}, {width, 15}, {label, {text, "25"}}, {font, {screen,bold, 12}}])),

	gs:create(rectangle, Canvas, [{coords, [{35, 50},{60, 80}]}, {bw, 1}, {fill, red}]),
	put(se,gs:create(label, Canvas, [{x, 40}, {y, 59}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 12}, {width, 15}, {label, {text, "25"}}, {font, {screen,bold, 12}}])),

	put(speedv,gs:create(label, WinHandle, [{x, 100}, {y, 35}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 25}, {width, 100}, {label, {text, "0 [km/h]"}}, {font, {screen,bold, 12}}])),


	gs:create(label, WinHandle, [{x, 5}, {y, 97}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 200}, {label, {text, "direct"}}, {font, {screen,bold, 10}}]),
	Canvas2 = gs:create(canvas, WinHandle, [{x, 10},{y, 110},{width, ?GPRAPH_WIDE},{height,90},{bg, green}]),
	put(direct,gs:create(line,Canvas2,[{coords,modlist({25,25,25,25})}, {arrow,last},{width,7},{fg,red}])),


%	gs:create(label, WinHandle, [{x, 100}, {y, 130}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 200}, {label, {text, "Stop time"}}, {font, {screen,bold, 10}}]),
%	put(stopt,gs:create(label, WinHandle, [{x, 100}, {y, 145}, {align, e}, {bw, 1}, {fg, red}, {bg, green}, {height, 25}, {width, 100}, {label, {text, "0 [mS]"}}, {font, {screen,bold, 12}}])),

	gs:create(label, WinHandle, [{x, 100}, {y, 80}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 200}, {label, {text, "<- Graph [km/h] (per 0.4 second) "}}, {font, {screen,bold, 10}}]),
	gs:create(label, WinHandle, [{x, 305}, {y, 185}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 30}, {label, {text, "0 "}}, {font, {screen,bold, 10}}]),
	gs:create(label, WinHandle, [{x, 305}, {y, 95}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 30}, {label, {text, "400"}}, {font, {screen,bold, 9}}]),
	Canvas3 = gs:create(canvas, WinHandle, [
									   {x, 100},	
									   {y, 97},
									   {width, ?GPRAPH_WIDE2},
									   {height,100},
									   {bg, green}]),
	ViewList = crelist(0,[]),
	put(speedgraph,gs:create(line,Canvas3,[{coords,ViewList}, {arrow,last},{width,2},{fg,red}])),

	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text, "quit"}} ,{width,80},{height,25}, {x,220},{y,35}])),

	put(parentPid, Parent),
	gs:config(WinHandle, {map, true}),
	put(balance,{25,25,25,25}),% 
	put(speed,0),% 
	event_loop(ViewList),
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
crelist(Count,List) when Count < ?GPRAPH_WIDE2->
		crelist(Count+1,[getgraphval({Count, 0}) | List]);
crelist(_,List) ->
		List.

getgraphval({Cnt,Val}) when Val >10000 -> {Cnt,0};
getgraphval({Cnt,Val}) -> {Cnt,100 - (Val div 4)}.


modlistS(New,List)-> 
		List2 = modlistS(lists:reverse(List)),
		[{Cnt,_}| Tail] = lists:reverse(List2),
		[getgraphval({Cnt,New}) | Tail].

modlistS([{Cnt1,_} , {Cnt2,Val2} ] )->
	[{Cnt1,Val2},{Cnt2,Val2} ]; 
modlistS([{Cnt1,_},{Cnt2,Val2} | T] )->
	[{Cnt1,Val2} | modlistS([{Cnt2,Val2}| T]) ].


modlist({Nw,Ne,_Se,_Sw}) when Nw =:=0 , Ne =:=0->
		[{35,50},{?GPRAPH_WIDE / 2,0}];
modlist({Nw,Ne,_Se,_Sw}) ->
		[{35,50},{?GPRAPH_WIDE * Nw/(Nw+Ne),0}].

reviewSpeedList(Sp,RpmList)->
	RpmList2 = modlistS(Sp,RpmList),
	gs:config(get(speedgraph), {coords, RpmList2}),
	RpmList2.

reviewBalance([Nw,Ne,Se,Sw])->
	Bl2 = modlist({Nw,Ne,Se,Sw}),
	gs:config(get(direct), {coords, Bl2}),
	gs:config(get(nw), {label, {text, Nw}}),
	gs:config(get(ne), {label, {text, Ne}}),
	gs:config(get(se), {label, {text, Se}}),
	gs:config(get(sw), {label, {text, Sw}}),
	put(balance,{Nw,Ne,Se,Sw}).

reviewSpeed(Sp)->
	gs:config(get(speedv), {label,{text, integer_to_list(Sp)++" [km/h]"}}),
	put(speed,Sp).

%reviewStopt(Sp)->
%	gs:config(get(stopt), {label,{text, integer_to_list(Sp)++" [mS]"}}),
%	put(speed,Sp).

event_loop(ViewList) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;
		{state_inf,paroutput, balance,Val} ->
			reviewBalance(Val),
			event_loop(ViewList);

		{state_inf,paroutput, speed,Val} ->
			ViewList2 = reviewSpeedList(Val,ViewList),
			reviewSpeed(Val),
			event_loop(ViewList2);

%		{state_inf,paroutput, stopt,Val} ->
%			reviewStopt(Val),
%			event_loop(ViewList);
		_ ->
			event_loop(ViewList)
	end.
