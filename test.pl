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
tails([_|T]) :- callable(T), call(T).

hello :- format("Hola!~n").

twofer(X) :- 0 #= mod(X,1), 5 #> X+3.



magicNumber(7).
magicNumber(9).
magicNumber(42).
