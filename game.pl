%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adventure Game
% (C) 2008 by Alex Oberhauser <OberhauserAlex@networld.to>
%
% This is a small and very stupid adventure game to deepen my prolog
% knowledge.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic light/1, thing/1, ything/1, at/2, bag/1, way/3, location_print/1,
           location/2, position/1, time/1, describe/1, use/1, look/0,
           lifeline/1, lifeline_nr/1, alive/0, death/0.
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
alive.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some general helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
status :-
        location_print(Location),
        position(Location2),
        lifeline(Lifeline),
        lifeline_nr(LifelineNr),
        findall(X, bag(X), Bag),
        findall(X, thing(X), Things),
        findall(X, ything(X), YThings),
        findall(X, way(_, X, _), Ways),
        print('-------------------------------------------------------------------'), nl,
        print('| LOCATION     | '), print(Location), print(' ('), print(Location2), print(')'), nl,
        print('| LIFELINE     | '), print_lifeline(Lifeline), print(' ('), print(LifelineNr), print(')'), nl,
        print('| BAG          | '), print_list(Bag), nl,
        print('| ENVIRONMENT  | '), print_list(Things), print_list(YThings), nl,
        print('| Ways         | '), print_list(Ways), nl,
        print('-------------------------------------------------------------------'), nl.

leave :-
        halt.

get_place(Place) :-
        location(Place, _).

get_place_desc(Placedesc) :-
        location(_, Placedesc).

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

print_list([]) :- !.
print_list([X|List]) :-
        print(X), print(', '),
        print_list(List).

print_lifeline([]) :- !.
print_lifeline([X|List]) :-
        print(X),
        print_lifeline(List).

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
        inc_lifeline,
        Y is X - 1,
        asserta(bag(package(first-aid-kit, Y))).

help :-
        print('************************************************************'), nl,
        print('use(something).        -> Use the object \'something\''), nl,
        print('inspect(something).    -> Inspect the objet \'something\''), nl,
        print('take(something).       -> Add \'something\' to your bag'), nl,
        print('drop(something).       -> Drop \'something\' from your bag'), nl,
        print('look.                  -> Look around you.'), nl,
        print('describe.              -> Describe the current place.'), nl,
        print('bag.                   -> List what you have in our bag.'), nl,
        print('n., s., w., e.         -> Go to north, south, west or east.'), nl,
        print('leave.                 -> Quit the game.'), nl,
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
        retractall(position(_)),
        retractall(describe(_)),
        retractall(look),
        retractall(use(_)),
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
        alive,
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
        alive, light,
        way(Oldlocation, north, Newlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to north.'), nl, !,
        look.

n :-    todark.

s :-
        alive, light,
        way(Oldlocation, south, Newlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to south.'), nl, !,
        look.

s :-    todark.

w :-
        alive, light,
        way(Oldlocation, west, Newlocation), !,
        retract(position(Oldlocation) :- !),
        asserta(position(Newlocation) :- !),
        print('You are gone to west.'), nl, !,
        look.

w :-    todark.

e :-
        alive, light,
        way(Oldlocation, east, Newlocation), !,
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
bag(package(first-aid-kit, 6)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here are the actions which you can take.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_location(Newlocation) :-
        retract(location_print(_) :- !),
        location(Newlocation, X),
        assert(location_print(X) :- !).

inspect(X) :-
        alive,
        findall(Y, at(Y, X), Z),
        add_things(Z),
        print('The following things are on the '),
        print(X), print(': '), print(Z).

add_things([]) :- !.
add_things([X|Xs]) :-
        asserta(thing(X)),
        add_things(Xs).

take(X) :-
        alive,
        at(X, Y), thing(X), !,
        retract(thing(X)),
        asserta(bag(X)),
        print('You have added the following thing from '),
        print(Y),
        print(' to your bag: '), print(X), nl.

take(X) :-
        alive,
        thing(X), tangible(X), !,
        retract(thing(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), nl.

take(X) :-
        alive,
        ything(X), !,
        retract(ything(X)),
        asserta(bag(X)),
        print('You have added the following thing to your bag: '),
        print(X), nl.

take(X) :-
        alive,
        print('You can\'t take the following thing: '),
        print(X), nl, !, fail.

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

open_door(X) :-
        alive,
        door(X), !,
        print('You have opened the following door: '),
        print(X), nl.
