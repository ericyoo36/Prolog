% Discussion on cuts!

% max (A, B, Output)
max1(A, B, B) :- B >= A, !.
max1(A, _, A).

max2(A, B, B) :- B >= A.
max2(A, B, A) :- B < A.

% myNot(X).
myNot(X) :- X, !, false.
myNot(_).

% not(X) = \+ X

% myForAll(X, Y).
% ~~(forall x. y) = exists x. ~y

myForAll(X, Y) :- X, myNot(Y), !, false.
myForAll(_, _).


/*
Movies 4 boys movie problem
There are 4 boys watching a movie on a couch, what is the name of each fo the boys, color of their shirts, favourite movie, and snacks. Where do they sit on the couch?

Joshua is at one of the ends.
The boy wearing the Black shirt is somewhere to the left of the youngest boy.
Joshua likes Horror movies.
The 14-year-old boy is at the third position.
The boy wearing the Red shirt is somewhere between the 13-year-old boy and the one who likes Action movies, in that order.
Daniel likes Thriller movies.
The boy who is going to eat Cookies is at one of the ends.
The boy wearing the Black shirt is exactly to the left of the one who likes Thriller movies.
The boy who is going to eat Crackers is exactly to the right of the boy who likes Comedy movies.
The boy wearing the Red shirt is somewhere between the boy who is going to eat Popcorn and Nicholas, in that order.
At one of the ends is the boy who likes Thriller movies.
Nicholas is somewhere between Joshua and Daniel, in that order.
At the first position is the boy wearing the Green shirt.
*/

% names
joshua.
daniel.
nicholas.

% ages
age_youngest.
age_13.
age_14.

% shirt color
black.
red.
green.

% genre of movie
thriller.
comedy.
horror.
action.

% snack
cookies.
crackers.
popcorn.

% PersonK = (Name, Age, Shirt Color, Genre, Snack)
% L = [ Person1, Person2, Person3, Person4 ]

name((Name, _, _, _, _), Name).
age((_, Age, _, _, _), Age).
shirtColor((_, _, ShirtColor, _, _), ShirtColor).
genre((_, _, _, Genre, _), Genre).
snack((_, _, _, _, Snack), Snack).

% Joshua is at one of the ends.
hint1([P, _, _, _]) :- name(P, joshua).
hint1([_, _, _, P]) :- name(P, joshua).


% The boy wearing the Black shirt is somewhere to the left of the youngest boy.
order(P1, P2, [ P1 | Rest]) :- member(P2, Rest).
order(P1, P2, [ _  | Rest])  :- order(P1, P2, Rest).

hint2(L) :- order(P1, P2, L), shirtColor(P1, black), age(P2, youngest).

% Joshua likes Horror movies.
hint3(L) :- member(P, L), name(P, joshua), genre(P, horror).


% The 14-year-old boy is at the third position.
hint4([_, _, P, _]) :- age(P, age_14).

% The boy wearing the Red shirt is somewhere between the 13-year-old boy and the one who likes Action movies, in that order.
hint5(L) :- order(P1, P2, L), order(P2, P3, L), shirtColor(P2, red), age(P1, age_13), genre(P3, action).

% Daniel likes Thriller movies.
hint6(L) :- member(P,L), name(P, daniel), genre(P, thriller).

% The boy who is going to eat Cookies is at one of the ends.
hint7([P, _, _, _]) :- snack(P, cookies).
hint7([_, _, _, P]) :- snack(P, cookies).

% The boy wearing the Black shirt is exactly to the left of the one who likes Thriller movies.
neighbor(P1, P2, [ P1, P2 | _]).
neighbor(P1, P2, [_ | Rest]) :- neighbor(P1, P2, Rest).

hint8(L) :- neighbor(P1, P2, L), shirtColor(P1, black), genre(P2, thriller).

% The boy who is going to eat Crackers is exactly to the right of the boy who likes Comedy movies.
hint9(L) :- neighbor(P1, P2, L), genre(P1, comedy), snack(P2, crackers).

% The boy wearing the Red shirt is somewhere between the (boy who is going to eat Popcorn) and (Nicholas), in that order.
hint10(L) :- order(P1, P2, L), order(P2, P3, L), shirtColor(P2, red), snack(P1, popcorn), name(P3, nicholas).

% At one of the ends is the boy who likes Thriller movies.
hint11([P, _, _, _]) :- genre(P, thriller).
hint11([_, _, _, P]) :- genre(P, thriller).

% Nicholas is somewhere between Joshua and Daniel, in that order.
hint12(L) :- order(P1, P2, L), order(P2, P3, L), name(P1, joshua), name(P2, nicholas), name(P3, daniel).

% At the first position is the boy wearing the Green shirt.
hint13([P, _, _, _]) :- shirtColor(P, green).

% calling all the hints and generating a solution.
solution(L) :-
    hint1(L),
    hint2(L),
    hint3(L),
    hint4(L),
    hint5(L),
    hint6(L),
    hint7(L),
    hint8(L),
    hint9(L),
    hint10(L),
    hint11(L),
    hint12(L),
    hint13(L).
