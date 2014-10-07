%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(distance_gui).

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
-define(GPRAPH_WIDE, 170).
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
						   {title, string:concat("distance : ", atom_to_list(Position))},
						   {width, 200},
						   {height,300},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),
	Canvas = gs:create(canvas, WinHandle, [
									   {x, 10},
									   {y, 0},
									   {width, 200},
									   {height, 200},
									   {bg, black}]),

	gs:create(polygon, Canvas, [{coords, [{65, 60}, {75, 57}, {95, 57}, {105, 60}]}, {fg, green}, {fill, green}]),
	gs:create(rectangle, Canvas, [{coords, [{65, 60}, {105, 120}]}, {fg, green}, {fill, green}]),

	gs:create(polygon, Canvas, [{coords, get_arroXY_fr(75,20)},
							  {bw, 1}, get_fill_option(Position, frsensor)]),
	gs:create(polygon, Canvas, [{coords,get_arroXY_bk(80,130)},
							  {bw, 1}, get_fill_option(Position, bksensor)]),
	gs:create(polygon, Canvas, [{coords, get_arroXY_ri(115,85)},
							  {bw, 1}, get_fill_option(Position, risensor)]),
	gs:create(polygon, Canvas, [{coords,get_arroXY_lf(25,85)},
							  {bw, 1}, get_fill_option(Position, lfsensor)]),
  	gs:create(label, WinHandle, [{x, 10}, {y, 180}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 100}, {label, {text, "[Parameters]"}}, {font, {screen, bold, 10}}]),
	put(distanceScale, gs:create(scale, WinHandle, [{x, 50}, {y, 200}, {orient, horizontal}, {fg, green}, {bg, black}, {pos, 0}, {range, {0, 100}}, {showvalue, true}, {text, "Distance (m)"}, {data, distance}, {pos, 0} ])),
	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text,"quit"}} ,{width,80},{height,25}, {x,10},{y,260}])),
	put(parentPid, Parent),
	put(position, Position),
	
	gs:config(WinHandle, {map, true}),
	event_loop(idle).


distanceGraf()->
	Canvas3 = gs:create(canvas, get(mainWindow), [{x, 0},{y, 125},{width, 200},{height,125},{bg, black}]),
	put(grapback,Canvas3),
	Canvas2 = gs:create(canvas, get(mainWindow), [{x, 25},{y, 150},{width, ?GPRAPH_WIDE},{height,100},{bg, green}]),
	DisList = crelist(0,[]),
  	gs:create(label, Canvas3, [{x, 0}, {y, 0}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 100}, {label, {text, "Distance graph"}}, {font, {screen, bold, 10}}]),
  	Dis = gs:create(label, Canvas3, [{x, 105}, {y, 0}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 50}, {label, {text, "[M]="++ erlang:integer_to_list(calets:getDefDistance())}}, {font, {screen, bold, 10}}]),
	put(distext,Dis),
  	gs:create(label, Canvas3, [{x, 0}, {y, 25}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 20}, {label, {text, "100"}}, {font, {screen, bold, 10}}]),
  	gs:create(label, Canvas3, [{x, 0}, {y, 55}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 40}, {width, 25}, {label, {text, "[M]"}}, {font, {screen, bold, 10}}]),
  	gs:create(label, Canvas3, [{x, 0}, {y, 105}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 20}, {label, {text, "  0"}}, {font, {screen, bold, 10}}]),
	Line = gs:create(line,Canvas2,[{coords,DisList}, {arrow,last},{width,2},{fg,red}]),
	put(grap,Line),
	put(can2,Canvas2),
	DisList.

desDistanceGraf()->
	gs:destroy(get(grapback)),
	gs:destroy(get(can2)).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_arroXY_fr(X,Y)->
	[{X, Y}, {X+10, Y-10}, {X+20, Y}, {X+15,Y},{X+15,Y+30},{X+5,Y+30},{X+5,Y}].

get_arroXY_bk(X,Y)->
	[{X, Y}, {X+10, Y},{X+10,Y+30},{X+15,Y+30},{X+5,Y+40},{X-5,Y+30},{X,Y+30}].
	
get_arroXY_ri(X,Y)->
	[{X, Y}, {X+30, Y},  {X+30,Y-5},{X+40,Y+5},{X+30,Y+15} ,  {X+30,Y+10},{X,Y+10}].
get_arroXY_lf(X,Y)->
	[{X-10,Y+5},{X,Y-5}, {X, Y}, {X+30, Y},{X+30,Y+10},{X,Y+10}, {X,Y+15}].

get_default_win_position(Position) ->
	case Position of
		frsensor ->
			{288, 30};
		bksensor ->
			{288, 350};
		risensor ->
			{500, 200};
		lfsensor ->
			{30, 200}
	end.

get_fill_option(_Position,_Comp) when _Position =:= _Comp ->
			{fill, red};
get_fill_option(_,_)->
			{fill, none}.


crelist(Count,List) when Count < ?GPRAPH_WIDE->
		crelist(Count+1,[getgraphval({Count, calets:getDefDistance()}) | List]);
crelist(_,List) ->
		List.

getgraphval({Cnt,Val}) when Val >100 -> {Cnt,0};
getgraphval({Cnt,Val}) when Val < 0 -> {Cnt,100};
getgraphval({Cnt,Val}) -> {Cnt,100 - Val}.

modlist(New,List)-> 
		List2 = modlist(lists:reverse(List)),
		[{Cnt,_}| Tail] = lists:reverse(List2),
		[getgraphval({Cnt,New}) | Tail].

modlist([{Cnt1,_},{Cnt2,Val2}] )->
	[{Cnt1,Val2},{Cnt2,Val2}]; 

modlist([{Cnt1,_},{Cnt2,Val2} | T] )->
	[{Cnt1,Val2} | modlist([{Cnt2,Val2}| T]) ].

review(Dis,DisList)->
	DisList2 = modlist(Dis,DisList),
	gs:config(get(grap), {coords, DisList2}),
	if
		is_integer(Dis) ->	
			gs:config(get(distext), {label, {text, "[M]=" ++ erlang:integer_to_list(Dis) }});
		is_float(Dis)->
%			gs:config(get(distext), {label, {text, "[M]=" ++ erlang:float_to_list(Dis)}});
			gs:config(get(distext), {label, {text, "[M]=" ++ erlang:integer_to_list(round(Dis))}});
		true->
			false
	end,
	DisList2.


event_loop(State) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow)),
			true;

		{gs, _, destroy, _, _} ->
			true;

		{gs, _, click, distance, [Rounds|_]} ->
			PPid = get(parentPid),
			if 
				is_pid(PPid)-> 
					PPid!{state_inf,chgui,Rounds};
				true -> true
			end,
			event_loop(State);

		{state_inf,pare,[distance,Dis]} when State =:= scenario->
			put(dislist,review(Dis,get(dislist))),
			event_loop(State);

		{scenario_req,pare,_}->
			put(dislist,distanceGraf()),
			event_loop(scenario);

		{scenario_res,pare,_}->
			desDistanceGraf(),
			event_loop(idle);

		Any ->
			io:format("distancegui unknown type received~n" ),
			erlang:display(Any),
			event_loop(State)
	end.
