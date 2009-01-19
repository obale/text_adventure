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
%
location(prison, 'Old Prison') :- !.

describe(prison) :-
        light,
        print('You are in a very, very dark room...'),  nl.

describe(prison) :-
        dark,
        print('Seams that you are in a very old prison.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The ways out of here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position(startpoint) :- !.

way(startpoint, north, irondoor) :- !.
way(irondoor, south, startpoint) :- !.
way(_, _, _) :- print('You can\'t go this direction'), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change here what you see in your enviornment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
look :-
        light(on), position(startpoint), !,
        remove_things,
        asserta(at(book, table)),
        asserta(at(pen, table)),
        asserta(thing(table)),
        asserta(thing(chair)),
        asserta(visible(chair)),
        asserta(visible(table)),
        asserta(tangible(book)),
        asserta(tangible(pen)),
        findall(X, thing(X), Things),
        findall(X, ything(X), YThings),
        print('You see the following stuff: '),
        print(Things), print(YThings), nl.

look :-
        light(on), position(irondoor), !,
        remove_things.

look :-
        light(on), !,
        remove_things.

look :-
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
        print('You don\'t need your knife at the moment.'), !, nl.

use(torch) :-
        light,
        switch_light,
        remove_things,
        hurt(1),
        print('You stupid guy, you have scorched your fingers, and now the torches are blown out!'), !, nl.

use(X) :-
        print('You can\'t use the following thing: '),
        print(X), !, fail.
