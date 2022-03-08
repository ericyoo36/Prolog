[X,2,Y] and [[],Z|[[]]]
X = []
Z = 2
Y = [[]]

[X,Y,Z] and [22|[[]|(23,24)]]
X = 22
Y = []
Z = [(23,24)]

[f(X,Y)|Z] and [Z|W]
f(X,Y) = Z
Z = W
fails

[X,Y,Z] and [X|(Y,X)]
fails


frev(X,Y):-shunt(X,[],Y).             %(1) :- (1.1)

shunt([],X,X).                        %(2)
shunt([H|T],S,Z):-shunt(T,[H|S],Z).   %(3) :- (3.1)

/*
goal: frev([a,b,c],R).
unify with (1) frev(X1,Y1)      success {[a,b,c]/X1, R/Y1}
    body(1.1) shunt(X1,[],Y1) {[a,b,c]/X1, R/Y1}
      shunt([a,b,c],[],R)
        unify with (2) shunt([],X2,X2)        fails ([a,b,c] can't be equal to [])
        unify with (3) shunt([H2|T2],S2,Z2)   success {[H2|T2]/[a,b,c], S2/[], Z2/R}
            body(3.1) shunt(T2,[H2|S2],Z2) {[H2|T2]/[a,b,c], S2/[], Z2/R}
                shunt([b,c],[a],R)
                    unify with (2) shunt([],X3,X3)        fails ([b,c] can't be equal to [])
                    unify with (3) shunt([H3|T3],S3,Z3)   success {[H3|T3]/[b,c], S3/[a], Z3/R}
                        body(3.1) shunt(T3,[H3|S3],Z3) {[H3|T3]/[b,c], S3/[a], Z3/R}
                            shunt([c],[b,a],R)
                                unify with (2) shunt([],X4,X4)        fails ([c] can't be equal to [])
                                unify with (3) shunt([H4|T4],S4,Z4)   success {[H4|T4]/[c], S4/[b,a], Z4/R}
                                    body(3.1) shunt(T4,[H4|S4],Z4) {[H4|T4]/[c], S4/[b,a], Z4/R}
                                        shunt([],[c,b,a],R)
                                            unify with (2) shunt([],X4,X4)        success {[]/[], X4/[c,b,a], X4/R}
                                            body empty: answer X4 = [c,b,a]       ** R = [c,b,a]  ;force backtracking
                                            unify with (3) shunt([H4|T4],S4,Z4)   fails ([] can't equal to [H4|T4])

*/
leaves([],_):-!.
leaves(f(X), Y):-leaves(X,Y), !.
leaves((X,Y,Z),L):-leaves(X,L), leaves(Y,L), leaves(Z,L), !.
leaves(X,Y):-append(X,Y,Z).


grow([],_).
grow([L,X):-growelement(L,X,1).

growelement([],_,_).
growelement([H|T],X,N):-replicate(H,N,L,0), growelement(T,Y,N1), append(L,Y,X), N is N+1.

replicate([],_,_,_).
replicate(X,N,L,G):-G < N, append(X,L,L), replicate(X,N,L,G1), G1 is G+1.


the number of leaves of a ternary tree is defined by
leaves Tip = 1
leaves(Node a t1 t2 t3) = leaves t1 + leaves t2 + leaves t3

the number of nodes of a ternary tree is defined by
nodes Tip = 0
nodes(Node a t1 t2 t3) = 1 + nodes t1 + nodes t2 + nodes t3

prove by structure induction
leaves t = 1 + 2*nodes t

leaves Tip = 1 = 1 + 0 = 1 + nodes Tip
leaves (Node a t1 t2 t3) = leaves t1 + leaves t2 + leaves t3
= 1 + 2*nodes t1 + 1 + 2*nodes t2 + 1 + 2*nodes t3
= 1 + 2*nodes(Node a t1 t2 t3)
