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

location(prisontract, 'Old Prison Tract') :- !.
describe(prisontract) :-
        light,
        print('You have left the prison cell. Now you are in the prison tract.'), nl,
        print('The sun is shining trough a little whole in the wall.').

position(startpoint_tract) :- !.

way(startpoint_tract, south, irondoor) :-
        open_way(irondoor), save_world, !,
        retract(location(_, _) :- !),
        asserta(location(prisoncell, 'The Prison Cell') :- !),
        load_world.

open_way(irondoor).
light(on).
