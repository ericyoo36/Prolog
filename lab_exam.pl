
isAscending([]).
isAscending([L | Ls]) :- isAscendingImpl(L, Ls).

isAscendingImpl(_, []).
isAscendingImpl(X, [L | Ls]) :- X =< L, isAscendingImpl(L, Ls).

myForall(X, Y) :- X, \+ Y, !, false.
myForall(_, _).

myLength([], 0).
myLength([_ | Rest], O1) :- myLength(Rest, O), O1 is 1 + O.

listLonger(L1, L2) :-
    myLength(L1, Len1),
    myLength(L2, Len2),
    Len1 >= Len2.

firstGroup(X, G) :- append(G, _, X), isAscending(G).


group([], []) :- !.
group(X, O) :-
    append(G1, Rest, X),  % [1,2,1], G1 = [1, 2], Rest = [1]
    isAscending(G1),      % [1,2], firstGroup([1,2,1], G) G = [1], G = [1,2]
    myForall(firstGroup(X, G), listLonger(G1, G)),
    group(Rest, O1),
    O = [G1 | O1].

append([], X, X).                               % (1)
append([X |Y], Z, [X | W]) :- append(Y, Z, W).  % (2) :- (2.1)

reverse([],[]).                                     % (3)
reverse([H|T],Y) :- reverse(T,W), append(W,[H],Y).  % (4) :- (4.1), (4.2)

/*
goal: reverse(X, [a,b,c]).
unify with (3) reverse([], [])          fail ([] does not unify with [a,b,c])
unify with (4) reverse([H1|T1], Y1)     success { [H1|T1]/X, [a,b,c]/Y1}
    body(4.1) reverse(T1, W1) { [H1|T1]/X, [a,b,c]/Y1}
            = reverse(T1, W1)
            unify with (3) reverse([], [])      success { []/T1, []/W1 }.
        (4.2) append(W1, [H1], Y1) { []/T1, []/W1, [H1]/X, [a,b,c]/Y1 } !!! important do not forget a substitution with a variable
                = append([], [H1], [a,b,c])
            unify with (1) append([], X, X)     fails ([H1] does not unify with [a,b,c])
            unify with (2) append([H10|T10], Z10, [X10, W10])   fails ([] does unify with [H10|T10])
        backtrack...
        (4.1) reverse(T1, W1) { [H1|T1]/X, [a,b,c]/Y1}
            = reverse(T1, W1)
            unify with (4) reverse([H2|T2],Y2)      success {  }.
                body (4.1) reverse(T2, W2) { [H2|T2]/T1, Y2/W1 }.
                    unify with (3) reverse([], []). success {[]/T2, []/W2}
                body (4.2) append(W2, [H2], Y2) { []/T2, []/W2 }
                    = append([], [H2], Y2)
                    unify with (1) append([], X11, X11)         success { [H2]/X11, [H2]/Y2 }
        (4.2) append(W1, [H1], Y1) {  [H2]/T1, [H2]/W1, [a,b,c]/Y1 }  { [H2]/X11, [H2]/Y2, []/T2, []/W2 }
              = append([H2], [H1], [a, b, c])
              unify (1) append([], X11, X11)                     fails
              unify (2) append([X11|Y11], Z11, [X11 | W11])      success { a/H2, []/Y11, [H1]/Z11, a/X11, [b,c]/W11 }
                body (2.1) append(Y11, Z11, W11) { a/H2, []/Y11, [H1]/Z11, a/X11, [b,c]/W11 }
                            =  append([], [H1], [b,c])
                            unify with (1) fails
                            unify with (2) fails
                body (4.2) append(W2, [H2], Y2) { []/T2, []/W2 }
                    = append([], [H2], Y2)
                    unify with (2) append([X11|Y11], Z11, [X11|W11])        fail { [] does not unify with [ X11 | W11 ] }
                backtrack further up until (4.1)
                body (4.1) reverse(T2, W2) { [H2|T2]/T1, Y2/W1 }.
                    unify with (4) reverse ...
                    eventually terminates
                ...
                X = [c, b, a]



*/
strange(S,T):-suffix(M,T),prefix(S,M).
strange([],_).

suffix(S,S).
suffix(S,[_|T]):-suffix(S,T).

prefix([H|S].[H|T]):-prefix(S,T).
prefix([H].[H|T]).


abcd

bcd
cd
d
