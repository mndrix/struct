:- use_module(library(struct)).

:- structure option(
    color:atom=red,
    size:integer,
    name=joe
).


:- use_module(library(tap)).

empty :-
    exists(option, Opt),
    defaults(Opt),
    struct:color(Opt, Color),
    Color == red,
    struct:size(Opt, Size),
    var(Size),  % no default
    struct:name(Opt, Name),
    Name == joe.

'non-default color' :-
    exists(option, Opt),
    struct:color(Opt, green),
    defaults(Opt),
    struct:color(Opt, Color),
    Color == green,
    struct:size(Opt, Size),
    var(Size),  % no default
    struct:name(Opt, Name),
    Name == joe.

'non-default name' :-
    exists(option, Opt),
    struct:name(Opt, william),
    defaults(Opt),
    struct:color(Opt, Color),
    Color == red,
    struct:size(Opt, Size),
    var(Size),  % no default
    struct:name(Opt, Name),
    Name == william.
