%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adventure Game
% (C) 2008 by Alex Oberhauser <OberhauserAlex@networld.to>
%
% This is a small and very stupid adventure game to deepen my prolog
% knowledge.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
:- dynamic lifeline/1.
:- dynamic lifeline_nr/1.
:- dynamic alive/0.
:- dynamic death/0.
:- dynamic ything/1.
:- dynamic bag/1.
:- dynamic location_print/1.

start :-
        help,
        nl,
        init_world,
        nl,
        status.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here are the settings which variate from time to time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
location_print(unknown) :- !.
location(prisoncell, 'Old Prison Cell') :- !.
alive.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some general helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help :-
        print('************************************************************'), nl,
        print('use(something).        -> Use the object \'something\''), nl,
        print('inspect(something).    -> Inspect the object \'something\''), nl,
        print('take(something).       -> Add \'something\' to your bag'), nl,
        print('take                   -> Take one thing at the time, if possible.'), nl,
        print('drop(something).       -> Drop \'something\' from your bag'), nl,
        print('look.                  -> Look around you.'), nl,
        print('describe.              -> Describe the current place.'), nl,
        print('bag.                   -> List what you have in our bag.'), nl,
        print('n., s., w., e.         -> Go to north, south, west or east.'), nl,
        print('leave.                 -> Quit the game.'), nl,
        print('************************************************************'), nl.

status :-
        location_print(Location),
        position(Location2),
        lifeline(Lifeline),
        lifeline_nr(LifelineNr),
        findall(X, bag(X), Bag),
        findall(X, thing(X), Things),
        findall(X, ything(X), YThings),
        print('----------------'), nl,
        print('| LOCATION     | '), print(Location), print(' ('), print(Location2), print(')'), nl,
        print('| LIFELINE     | '), print_lifeline(Lifeline), print(' ('), print(LifelineNr), print(')'), nl,
        print('| BAG          | '), print_list(Bag), nl,
        print('| ENVIRONMENT  | '), print_list(Things), print_list(YThings), nl,
        print('----------------'), nl.

leave :-
        halt.

get_place(Place) :-
        location(Place, _).

get_place_desc(Placedesc) :-
        location(_, Placedesc).

rand_true(X) :-
        Y is random(X),
        1 = Y.

bag :-
        alive,
        findall(X, bag(X), Bag),
        print('You have the following stuff in your bag: '), nl,
        print(Bag).

lifeline(['+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
          '+', '+', '+', '+', '+', '+', '+', '+', '+', '+']) :- !.

lifeline_nr(20).

inc_lifeline :-
        lifeline_nr(Z),
        W is Z + 1,
        retract(lifeline_nr(Z)),
        asserta(lifeline_nr(W)).

dec_lifeline :-
        lifeline_nr(Z),
        W is Z - 1,
        retract(lifeline_nr(Z)),
        asserta(lifeline_nr(W)).

hurt(0) :- !.

hurt(_) :-
        lifeline([]), !,
        retract(alive),
        assert(death),
        print('You are death').

hurt(X) :-
        lifeline([_|Lifeline]),
        retract(lifeline([_|Lifeline]) :- !),
        asserta(lifeline(Lifeline) :- !),
        dec_lifeline,
        Y is X - 1,
        hurt(Y).

heal :-
        alive,
        bag(package(first-aid-kit, 0)), !,
        print('Your first-aid-kit is finished.'), nl.

heal :-
        alive,
        retract(lifeline(Lifeline) :- !),
        asserta(lifeline(['+'|Lifeline]) :- !),
        retract(bag(package(first-aid-kit, X))),
        inc_lifeline, !,
        Y is X - 1,
        asserta(bag(package(first-aid-kit, Y))).

heal :-
        print('You can\'t heal you, maybe you have not first-aid package or you are death.'), nl, !.

print_list([]) :- !.
print_list([X|List]) :-
        print(X), print(', '),
        print_list(List).

print_lifeline([]) :- !.
print_lifeline([X|List]) :-
        print(X),
        print_lifeline(List).

create_file(File) :-
        location(X, _),
        concat('places/', X, Tmpfile),
        concat(Tmpfile, '.pl', File).

remove_things :-
        retractall(at(_, _)),
        retractall(thing(_)),
        retractall(tangible(_)).

clean :-
        abolish(location/2),
        abolish(position/1),
        abolish(describe/1),
        abolish(look/0),
        abolish(close_way/1),
        abolish(open_way/1),
        abolish(light/1),
        abolish(use/1),
        abolish(way/3),
        remove_things.

init_world :-
        create_file(File),
        clean,
        consult(File),
        update_location(X),
        describe(X), !.

init_world :-
        create_file(File),
        print('The new world couldn\'t be loaded. Please check the file \''),
        print(File),
        print('\'.').

describe :-
        alive,
        location(X, _),
        status,
        describe(X).

light :- light(on), !.
dark :- light(off), !.

todark :-
        dark,
        print('It\'s to dark to take this action.').

switch_light :-
        light(off), !,
        retract(light(off) :- !),
        asserta(light(on) :- !).

switch_light :-
        light(on), !,
        retract(light(on) :- !),
        asserta(light(off) :- !).

n :-
        alive, light,
        position(Oldlocation),
        way(Oldlocation, north, Newlocation), open_way(Oldlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to north.'), nl, !,
        look.

n :-    todark.

s :-
        alive, light,
        position(Oldlocation),
        way(Oldlocation, south, Newlocation), open_way(Oldlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to south.'), nl, !,
        look.

s :-    todark.

w :-
        alive, light,
        position(Oldlocation),
        way(Oldlocation, west, Newlocation), open_way(Oldlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to west.'), nl, !,
        look.

w :-    todark.

e :-
        alive, light,
        position(Oldlocation),
        way(Oldlocation, east, Newlocation), open_way(Oldlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        look,
        print('You are gone to east.'), nl, !.

e :-    todark.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff which you have in your bag.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bag(knife).
bag(lighter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here are the actions which you can take.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_location(Newlocation) :-
        retract(location_print(_) :- !),
        location(Newlocation, X),
        assert(location_print(X) :- !).

inspect(X) :-
        alive,
        findall(Y, at(Y, X), []),
        print('You can\'t inspect this thing or all things are taken from this place.'), !, nl.

inspect(X) :-
        alive,
        findall(Y, at(Y, X), Z),
        add_things(Z),
        print('The following things are on the '),
        print(X), print(': '), print(Z).

add_things([]) :- !.

add_things([X|_]) :-
        findall(Z, thing(Z), Zs),
        member(X, Zs), !.

add_things([X|Xs]) :-
        asserta(thing(X)),
        add_things(Xs).

take :-
        take(_).

take(X) :-
        alive,
        at(X, Y), thing(X), tangible(X), !,
        retract(thing(X)),
        retract(at(_, _)),
        asserta(bag(X)),
        print('You have added the following thing from '),
        print(Y),
        print(' to your bag: '), print(X), !, nl.

take(X) :-
        alive,
        thing(X), tangible(X), !,
        retract(thing(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), !, nl.

take(X) :-
        alive,
        ything(X), !,
        retract(ything(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), !, nl.

take(_) :-
        alive,
        print('There is nothing what you can take.'), nl, !, fail.

drop(X) :-
        alive,
        bag(X), !,
        retract(bag(X)),
        asserta(ything(X)),
        print('You have droped the following thing to the environment: '), print(X).

drop(X) :-
        alive,
        print('The following don\'t exists in our bag: '),
        print(X), nl, !, fail.

open_way :-
        alive,
        position(X),
        close_way(X), !,
        retract(close_way(X)),
        asserta(open_way(X)),
        print('You have opened the following door: '),
        print(X), nl.
