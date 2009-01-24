:- dynamic location/2.

location(prisoncell, 'Old Prison Cell') :- !.

:- dynamic location_print/1.

location_print('Old Prison Cell') :- !.

:- dynamic position/1.

position(irondoor) :- !.

:- dynamic describe/1.

describe(prisoncell) :-
	dark,
	print('You are in a very, very dark room...'), !,
	nl.
describe(prisoncell) :-
	light,
	print('Seams that you are in a very old prison cell.'), !,
	nl.

:- dynamic look/0.

look :-
	light,
	position(startpoint), !,
	remove_things,
	asserta(at(book, table)),
	asserta(at(pen, table)),
	asserta(at(package(first-aid-kit, 20), table)),
	asserta(thing(table)),
	asserta(thing(chair)),
	asserta(tangible(book)),
	asserta(tangible(pen)),
	asserta(tangible(package(first-aid-kit, 20))),
	findall(A, thing(A), B),
	findall(A, ything(A), C),
	print('You see the following stuff: '),
	print_list(B),
	print_list(C),
	nl.
look :-
	light,
	position(irondoor), !,
	remove_things.
look :-
	light, !,
	remove_things.
look :-
	dark,
	remove_things,
	print('It\'s to dark to see something.'), !,
	fail.

:- dynamic close_way/1.


:- dynamic open_way/1.

open_way(startpoint_tract).
open_way(irondoor).
open_way(startpoint).

:- dynamic light/1.

light(on) :- !.

:- dynamic use/1.

use(lighter) :-
	dark,
	thing(torch),
	print('You have seen two torchs and light it.'),
	nl,
	switch_light,
	update_location(_),
	look,
	asserta(thing(torch)),
	status, !.
use(lighter) :-
	dark,
	asserta(thing(torch)),
	print('In the small shine of your lighter you see torches.'),
	nl,
	print('To light it use the lighter a second time.'),
	nl, !.
use(lighter) :-
	light,
	print('Its not dark and there is no other use of the lighter.'), !,
	nl.
use(knife) :-
	position(irondoor),
	close_way(startpoint_tract),
	rand_true(2),
	openway(startpoint_tract), !.
use(knife) :-
	position(irondoor),
	close_way(startpoint_tract),
	print('Trying to open the door with my knife...'),
	nl,
	sleep(1),
	print('Seams to be hard work to open a iron door with my knife. But lets try another time.'), !,
	nl.
use(knife) :-
	position(irondoor),
	open_way(startpoint_tract), !,
	print('The door is open, you don\'t need your tool.'), !,
	fail,
	nl.
use(knife) :-
	print('You don\'t need your knife at the moment.'), !,
	fail,
	nl.
use(torch) :-
	light,
	switch_light,
	remove_things,
	hurt(1),
	print('You stupid guy, you have scorched your fingers, and now the torches are blown out!'), !,
	nl.
use(A) :-
	print('You can\'t use the following thing: '),
	print(A), !,
	fail.

way(startpoint, north, irondoor) :- !.
way(irondoor, south, startpoint) :- !.
way(irondoor, north, startpoint_tract) :-
	open_way(startpoint_tract), !,
	save_world,
	retract((location(_, _):-!)),
	asserta((location(prisontract, 'The Prison Tract'):-!)),
	init_world.
way(_, _, _) :-
	print('You can\'t go this direction. Please look if there is a door or other entry which you can open.'), !,
	fail.

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

