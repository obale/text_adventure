%
% Adventure Game
% (C) 2008 by Alex Oberhauser <OberhauserAlex@networld.to>
%
% This is a small and very stupid adventure game to deepen my prolog
% knowledge.
%
% CHECKLIST
% ---------
%
% [x] location/2      (@1: place; @2: 'Place Description')
% [x] position/1      (@1: place in a room)
% [x] describe/1      (@1: place)
% [x] look/0 
% [x] use/1           (@1: object)
% [x] way/3           (@1: source; @2: direction; @2: destination)
% [X] close_way/1     (@1: the door which is open or closed)
%     open_way/1
:- dynamic thing/1.
:- dynamic at/2.
:- dynamic location/2.
:- dynamic position/1.
:- dynamic describe/1.
:- dynamic use/1.
:- dynamic look/0.
:- dynamic light/1.
:- dynamic open_way/1.
:- dynamic close_way/1.

location(prisoncell, 'Old Prison Cell') :- !.

describe(prisoncell) :-
        dark,
        print('You are in a very, very dark room...'),  !, nl.

describe(prisoncell) :-
        light,
        print('Seams that you are in a very old prison cell.'), !, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The ways out of here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position(startpoint) :- !.

way(startpoint, north, irondoor) :- !.
way(irondoor, south, startpoint) :- !.
/*
way(irondoor, north, startpoint_tract) :-
        open_way(irondoor), !,
        retract(location(_, _) :- !),
        asserta(location(prisontract, 'The Prison Tract') :- !),
        init_world.
*/

way(_, _, _) :- print('You can\'t go this direction. Please look if there is a door or other entry which you can open.'), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some states of different things.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
light(off) :- !.
close_way(irondoor).
open_way(startpoint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change here what you see in your enviornment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
look :-
        light, position(startpoint), !,
        remove_things,
        asserta(at(book, table)),
        asserta(at(pen, table)),
        asserta(at(package(first-aid-kit, 20), table)),
        asserta(thing(table)),
        asserta(thing(chair)),
        asserta(tangible(book)),
        asserta(tangible(pen)),
        asserta(tangible(package(first-aid-kit, 20))),
        findall(X, thing(X), Things),
        findall(X, ything(X), YThings),
        print('You see the following stuff: '),
        print_list(Things), print_list(YThings), nl.

look :-
        light, position(irondoor), !,
        remove_things.

look :-
        light, !,
        remove_things.

look :-
        dark,
        remove_things,
        print('It\'s to dark to see something.'),
        !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use function for the single things.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use(lighter) :-
        dark,
        thing(torch),
        print('You have seen two torchs and light it.'), nl,
        switch_light,
        update_location(_),
        look,
        asserta(thing(torch)),
        status, !.

use(lighter) :-
        dark,
        asserta(thing(torch)),
        print('In the small shine of your lighter you see torches.'), nl,
        print('To light it use the lighter a second time.'), nl, !.

use(lighter) :-
        light,
        print('It\s not dark and there is no other use of the lighter.'), !, nl.

use(knife) :-
        position(irondoor), close_way(irondoor),
        rand_true(4),
        open_way, !.

use(knife) :-
        position(irondoor), close_way(irondoor),
        print('Trying to open the door with my knife...'), nl,
        sleep(1),
        print('Seams to be hard work to open a iron door with my knife. But lets try another time.'), !, nl.

use(knife) :-
        position(irondoor), open_way(irondoor), !,
        print('The door is open, you don\'t need your tool.'), !, fail, nl.

use(knife) :-
        print('You don\'t need your knife at the moment.'), !, fail, nl.

use(torch) :-
        light,
        switch_light,
        remove_things,
        hurt(1),
        print('You stupid guy, you have scorched your fingers, and now the torches are blown out!'), !, nl.

use(X) :-
        print('You can\'t use the following thing: '),
        print(X), !, fail.
