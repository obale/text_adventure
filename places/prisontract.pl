/*************************************************************************
 * Adventure Game - prisontract.pl 
 *
 * (C) 2008
 * Written by Alex Oberhauser <oberhauseralex@networld.to>
 * All Rights Reserved
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software.  If not, see <http://www.gnu.org/licenses/>
 **************************************************************************
 * CHECKLIST
 * ---------
 *
 * [x] location/2      (@1: place; @2: 'Place Description')
 * [x] position/1      (@1: place in a room)
 * [x] describe/1      (@1: place)
 * [x] look/0
 * [x] use/1           (@1: object)
 * [x] way/3           (@1: source; @2: direction; @2: destination)
 * [X] close_way/1     (@1: the door which is open or closed)
 *     open_way/1
 *************************************************************************/
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
        light, position(startpoint_tract),
        print('You have left the prison cell. Now you are in the prison tract.'), nl,
        print('The sun is shining trough a little whole in the wall.').

position(startpoint_tract) :- !.

way(startpoint_tract, south, irondoor) :-
        open_way(irondoor), save_world, !,
        retract(location(_, _) :- !),
        asserta(location(prisoncell, 'The Prison Cell') :- !),
        load_world.

look :-
        asserta(thing(sword)),
        asserta(tangible(sword)).

use(sword) :-
        print('You can\'t use this sword at the moment.'), nl, !.

open_way(irondoor).
light(on).
