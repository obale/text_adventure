%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adventure Game
% (C) 2008 by Alex Oberhauser <OberhauserAlex@networld.to>
%
% This is a small and very stupid adventure game to deepen my prolog
% knowledge.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic light/1, thing/1, ything/1, at/2, bag/1, way/2, location_print/1,
           location/2, location_room/1, time/1, describe/1, use/1, look/0.
:- multifile use/1, location/2.

start :-
        help,
        nl,
        init_world,
        nl,
        status.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here are the settings which variate from time to time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
light(off) :- !.
location_print(unknown) :- !.
location(prison, 'Old Prison') :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some general helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
status :-
        location_print(Location),
        location_room(Location2),
        findall(X, bag(X), Bag),
        findall(X, thing(X), Things),
        findall(X, ything(X), YThings),
        findall(X, way(X, _), Ways),
        print('-------------------------------------------------------------------'), nl,
        print('LOCATION:               '), print(Location), print(' -> '), print(Location2), nl,
        print('THINGS in your bag:     '), print(Bag), nl,
        print('THINGS in this place:   '), print(Things), print(YThings), nl,
        print('Ways to the next rooms: '), print(Ways), nl,
        print('-------------------------------------------------------------------'), nl.

bag :-
        findall(X, bag(X), Bag),
        print('You have the following stuff in your bag: '), nl,
        print(Bag).

help :-
        print('************************************************************'), nl,
        print('use(something).        -> Use the object \'something\''), nl,
        print('inspect(something).    -> Inspect the objet \'something\''), nl,
        print('take(something).       -> Add \'something\' to your bag'), nl,
        print('drop(something).       -> Drop \'something\' from your bag'), nl,
        print('look.                  -> Look around you.'), nl,
        print('************************************************************'), nl.

create_file(File) :-
        location(X, _),
        concat('places/', X, Tmpfile),
        concat(Tmpfile, '.pl', File).

remove_things :-
        retractall(at(_, _)),
        retractall(thing(_)),
        retractall(tangible(_)).

clean :-
        retractall(location(_, _)),
        retractall(look),
        remove_things.

init_world :-
        create_file(File),
        clean,
        consult(File),
        location(X, _),
        describe(X), !.

init_world :-
        create_file(File),
        print('The new world couldn\'t be loaded. Please check the file \''),
        print(File),
        print('\'.').

describe :-
        location(X, _),
        status,
        describe(X).

light :- light(on), !.
dark :- light(off), !.

todark :-
        dark,
        print('It\'s to dark to take this action').

switch_light :-
        light(off), !,
        retract(light(off) :- !),
        asserta(light(on) :- !).

switch_light :-
        light(on), !,
        retract(light(on) :- !),
        asserta(light(off) :- !).

n :-
        light,
        way(Oldlocation, north, Newlocation), !,
        retract(location_room(Oldlocation) :- !),
        asserta(location_room(Newlocation) :- !),
        print('You are gone to north.'), nl, !,
        look.

n :-    todark.

s :-
        light,
        way(Oldlocation, south, Newlocation), !,
        retract(location_room(Oldlocation) :- !),
        asserta(location_room(Newlocation) :- !),
        print('You are gone to south.'), nl, !,
        look.

s :-    todark.

w :-
        light,
        way(Oldlocation, west, Newlocation), !,
        retract(location_room(Oldlocation) :- !),
        asserta(location_room(Newlocation) :- !),
        print('You are gone to west.'), nl, !,
        look.

w :-    todark.

e :-
        light,
        way(Oldlocation, east, Newlocation), !,
        retract(location_room(Oldlocation) :- !),
        asserta(location_room(Newlocation) :- !),
        look,
        print('You are gone to east.'), nl, !.

e :-    todark.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff which you have in your bag.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bag(knife).
bag(lighter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This are possible only test data. Copy this part then to the use(light)
% statement.
%
% Here are the actions which you can take.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_location(Newlocation) :-
        retract(location_print(_) :- !),
        location(Newlocation, X),
        assert(location_print(X) :- !).

inspect(X) :-
        findall(Y, at(Y, X), Z),
        add_things(Z),
        print('The following things are on the '),
        print(X), print(': '), print(Z).

add_things([]) :- !.
add_things([X|Xs]) :-
        asserta(thing(X)),
        add_things(Xs).

take(X) :-
        at(X, Y), thing(X), !,
        retract(thing(X)),
        asserta(bag(X)),
        print('You have added the following thing from '),
        print(Y),
        print(' to your bag: '), print(X), nl.

take(X) :-
        thing(X), tangible(X), !,
        retract(thing(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), nl.

take(X) :-
        ything(X), !,
        retract(ything(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), nl.

take(X) :-
        print('You can\'t take the following thing: '),
        print(X), nl, !, fail.

drop(X) :-
        bag(X), !,
        retract(bag(X)),
        asserta(ything(X)),
        print('You have droped the following thing to the environment: '), print(X).

drop(X) :-
        print('The following don\'t exists in our bag: '),
        print(X), nl, !, fail.

open_door(X) :-
        door(X), !,
        print('You have opened the following door: '),
        print(X), nl.
