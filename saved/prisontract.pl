:- dynamic location/2.

location(prisontract, 'Old Prison Tract') :- !.

:- dynamic location_print/1.

location_print('Old Prison Tract') :- !.

:- dynamic position/1.

position(startpoint_tract) :- !.

:- dynamic describe/1.

describe(prisontract) :-
	light,
	print('You have left the prison cell. Now you are in the prison tract.'),
	nl,
	print('The sun is shining trough a little whole in the wall.').

:- dynamic look/0.


:- dynamic close_way/1.


:- dynamic open_way/1.

open_way(irondoor).

:- dynamic light/1.

light(on).

:- dynamic use/1.


way(startpoint_tract, south, irondoor) :-
	open_way(irondoor),
	save_world, !,
	retract((location(_, _):-!)),
	asserta((location(prisoncell, 'The Prison Cell'):-!)),
	load_world.

:- dynamic bag/1.

bag(package(first-aid-kit, 20)).
bag(knife).
bag(lighter).

:- dynamic thing/1.


:- dynamic ything/1.


:- dynamic at/2.


:- dynamic tangible/1.


:- dynamic lifeline/1.

lifeline([+, +, +, +, +, +, +, +, +, +, +, +, +, +, +, +, +, +, +, +]) :- !.

:- dynamic lifeline_nr/1.

lifeline_nr(20).

