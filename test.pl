:- use_module(library(clpfd)).

map(FunctionName,[H|T],[AnsH|AnsT]):-
    Function=..[FunctionName,H,AnsH],
    call(Function),
    map(FunctionName,T,AnsT).
map(_,[],[]).

partition(Pred, Ls0, As, Bs) :-
        include(Pred, Ls0, As),
        exclude(Pred, Ls0, Bs).

neg(A,Ans) :- Ans is -A.
is_odd(I) :-
    0 #\= I mod 2.
is_even(I) :-
    0 #= I mod 2.
is_three(I) :-
    0 #= I mod 3.

heads([]) :- false.
heads([H]) :- callable(H), call(H).
heads([H|_]) :- callable(H), call(H).

tails([]) :- false.
tails([T]) :- callable(T), call(T).
tails([_|[T]]) :- callable(T), call(T).

hello :- format("Hola!~n").

twofer(X) :- 0 #= mod(X,1), 5 #> X+3.



magicNumber(7).
magicNumber(9).
magicNumber(42).

solveMoney(SEND,MORE,MONEY) :-
    [S,E,N,D,M,O,R,Y] ins 0..9, S #\= 0, M #\= 0,
    all_different([S,E,N,D,M,O,R,Y]),
    S * 1000 + E * 100 + N * 10 + D + M * 1000 + O * 100 + R * 10 + E #= M * 10000 + O * 1000 + N * 100 + E * 10 + Y,
    SEND #= S*1000 + E*100 + N*10 + D,
    MORE #= M * 1000 + O * 100 + R * 10 + E,
    MONEY #= M * 10000 + O * 1000 + N * 100 + E * 10 + Y,
    label([SEND,MORE,MONEY]).

%% solveMoney(SEND,MORE,MONEY) :-
%%     [S,E,N,D,M,O,R,Y] ins 0..9, S #\= 0, M #\= 0,
%%     all_different([S,E,N,D,M,O,R,Y]),
%%     SEND #= S*1000 + E*100 + N*10 + D,
%%     MORE #= M * 1000 + O * 100 + R * 10 + E,
%%     MONEY #= M * 10000 + O * 1000 + N * 100 + E * 10 + Y,
%%     MONEY #= SEND + MORE.

height(tip, 0).
height(bin(L,_,R), H) :-
    H #> 0,
    H #= 1 + max(LH,RH),
    height(L, LH),
    height(R, RH).

bst(T) :- ftree(T), bst(_,T,_).
bst(Lo, tip, Hi) :-
    Lo #< Hi.
bst(Lo, bin(L,X,R), Hi) :-
    bst(Lo, L, X),
    bst(X, R, Hi).

ftree(T) :- freeze(T, ftree_(T)).
ftree_(tip).
ftree_(bin(L,_,R)) :- ftree(L), ftree(R).

summer([], 0).
summer([X|Xs],T) :- T #= S + X, summer(Xs,S).

bst_member(X, bin(L, X, R)).
bst_member(X, bin(L, Y, _)) :- X #< Y, bst_member(X, L).
bst_member(X, bin(_, Y, R)) :- X #> Y, bst_member(X, R).
