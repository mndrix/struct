:- use_module(library(struct)).

:- structure point(x,y).

:- use_module(library(tap)).

struct :-
    exists(point, P),
    must_be(struct, P).

'typed struct' :-
    exists(point, P),
    must_be(struct(point), P).

'plain type' :-
    exists(point, P),
    must_be(point, P).

