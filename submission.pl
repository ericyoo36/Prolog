% CPSC449 Winter 2021
% Prolog Exercise #1
% Seongmok, Yoo (Eric)
% 10162624

% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
:- module(submission, []).
:- use_module(library(lists), []).
% Begin Assignment Code

%%%%%%% Q1 %%%%%%%

% myappend which succeeds when Z is the result of appending X and Y
% example code used from Alex tutorial section
% X concatenated with Y results in Z
% myappend(X, Y, Z).
myappend([],[],[]).
myappend([],L,L).
myappend(L,[],L).
myappend([X|L1],L2,[X|L3]):-myappend(L1,L2,L3).

% myreverse which succeeds when Y is the result of reversing X
% example code used from textbook "Programming in Prolog" chapter 7.5
% Y is the reverse of X
% reverse(X, Y).
myreverse([],[]).
myreverse([H|T],L):-myreverse(T,Z), myappend(Z,[H],L).

% myflatten which succeeds when Y is the result of flattening X
% Y is the flattening of X
% myflatten(X, Y).
myflatten([],[]).
myflatten([A|B],[X|Y]):-myflatten(B,Y).
myflatten([A|B],Y):-myflatten(A,Z), myflatten(B,T), myappend(Z,T,Y).

% mymemberwhich succeeds when element X is exists in Y
% example code used from textbook "Programming in Prolog" chapter 7.5
% X is a member of Y
% mymember(X, Y).
mymember(X, [X|_]).
mymember(X, [_|Y]):-mymember(X,Y).

% myremove which succeeds when Z is the result of removing X from Y
% example code used from textbook "Programming in Prolog" chapter 7.5
% Z is list obtained from remove X from Y
% myremove(X, Y, Z).
myremove(A,[A|L],L):-!.
myremove(A, [B|L], [B|M]):-myremove(A,L,M).

%%%%%%% Q2 %%%%%%%
% mymember2 which succeeds when X is occuring only twice in given list L
% X occurs precisely two times in L
% mymember2(X, L).
mymember2(X,[X|Y]):-mymember1(X,Y).
mymember2(X,[_|Y]):-mymember2(X,Y).

mymember1(X, [X|Y]):- myNot(mymember(X,Y)).
mymember1(X, [_|Y]):-mymember1(X,Y).

myNot(X) :- X, !, false.
myNot(_).


%%%%%%% Q3 %%%%%%%
% substring which succeeds when X is present in Y as a substring
% example code used from textbook "Programming in Prolog" chapter 7.5
% X is a contiguous sublist of Y
% substring(X, Y).
substring(X,Y):-myappend(_,T,Y), myappend(X,_,T).

%%%%%%% Q4 %%%%%%%
% sublists which displays all of sublists of given list L
% example code used Si Zhang tutorial section
% O contains all the sublists of L
% sublists(L, O).
sublists([],[]).
sublists([H|T],[H|R]):-sublists(T,R).
sublists([_|T],R):-sublists(T,R).


%%%%%%% Q5 %%%%%%%
% mypermutation which succeeds when X,Y have equal length and equal elements in different order
% example code used leture notes "Reversing and permutating"
% X and Y are permutations of each other.
% mypermutation(X, Y).
mypermutation(X,Y):-samelength(X,Y),perm(X,Y).

samelength([],[]).
samelength([_|X],[_|Y]):-samelength(X,Y).

perm([],[]).
perm([H|T],Z):-perm(T,S),insert(H,S,Z).

insert(H,T,[H|T]).
insert(H,[X|T],[X|Z]):-insert(H,T,Z).

%%%%%%% Q6 %%%%%%%
% Family question where different family relations are states using the definition given

% Note, the daughter and son predicates are assumed to be in this form.
%
% daughter(Mother, Father, Daughter)
% son(Mother, Father, Son)
%
son('Michelle', 'Jimmy', 'Eric').
daughter('Michelle', 'Jimmy', 'Riley').

% grandfather which succeeds A is a father of X and X is a parent of B
% is Grandfather a grandfather of Child
% grandfather(Grandfather, Grandchild).
grandfather(A,B):-(son(_,A,X) ; daughter(_,A,X)), (son(_,X,B) ; son(X,_,B) ; daughter(_,X,B) ; daughter(X,_,B)).

% grandmother which succeeds A is a mother of X and X is a parent of B
% is Grandmother a grandmother of Child
% grandmother(Grandmother, Grandchild).
grandmother(A,B):-(son(A,_,X) ; daughter(A,_,X)), (son(X,_,B) ; son(_,X,B) ; daughter(_,X,B); daughter(X,_,B)).

% brother which succeeds when A is a son of mother X or father Y, and B is a child of mother X or father Y
% is Brother a brother of Child
% brother(Brother, Child).
% A is a brother to B IF A is a son and has parenst (X,Y) AND (B is a son OR daughter OR has parents (X,Y)).
brother(A,B):-((son(X,_,A), (son(X,_,B) ; daughter(X,_,B))) ; (son(_,Y,A), (son(_,Y,B) ; daughter(_,Y,B)))), \+ (A=B).

% sister which succeeds when A is a daughter of mother X or father Y, and B is a child of mother X or father Y
% is Sister a sister of Child
% sister(Sister, Child).
sister(A,B):-((daughter(X,_,A), (son(X,_,B) ; daughter(X,_,B))) ; (daughter(_,Y,A), (son(_,Y,B) ; daughter(_,Y,B)))), \+ (A=B).

% sibling which succeeds when A and B are brothers or sisters
% Sibling a sibling of child
% sibling(Sibling, Child).
sibling(A,B):-brother(A,B) ; sister(A,B).

% cousins which succeeds when A is a child of parent X, Y, and B is a child of Z,B, and there exists a sibling between any pair of XYZB
% Is A a cousin of B?
% I.e. (Is A a child of a sibling of B).
% cousin(A, B).
cousin(A,B):-(son(X,_,A) ; son(_,X,A) ; daughter(X,_,A) ; daughter(_,X,A)), sibling(Z,X), (son(Z,_,B) ; son(_,Z,B) ; daughter(Z,_,B) ; daughter(_,Z,B)).

%%%%%%% Q7 %%%%%%%

% Note, an edge is defined as
%   edge(X, Y).
% which is a unidirectional edge from X to Y

% path which succeeds when from X to Y there is a path available to travel in given graph
% Does there exists a path between X and Y
% path(X, Y).
path(X,X).
path(X,Y):-edge(X,Z), path(Z,Y).

% shortpath which returns the length of the shortest path from X to Y in given graph
% What is the length L of the shortest path from X to Y
% shortpath(X, Y, L).
shortpath(X,Y,1):-edge(X,Y).
shortpath(X,Y,Len):-edge(X,Z),shortpath(Z,Y,Len1),Len is Len1+1.
