:- use_module(library(struct)).

:- structure point(x,y).

% describe struct facts with dict notation
point{x: 0,    y: 0}.
point{x: 1,    y: 42}.
point{y: -3.2, x: 18}.

:- use_module(library(tap)).

'all facts exist' :-
    exists(point, Point),
    findall(Point,in_db(Point),Points),
    length(Points, N),
    N == 3.

origin :-
    exists(point,Point,[x-0,y-0]),
    in_db(Point).
