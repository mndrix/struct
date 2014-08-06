:- use_module(library(struct)).

:- structure hello(whom:string, lang:atom).

sample(X) :-
    exists(hello, X),
    struct:whom(X,"Michael"),
    struct:lang(X,en).

:- use_module(library(tap)).


'there is a structure' :-
    current_structure(hello).

'structures of this type exist' :-
    exists(hello, Hello),
    exists(Name, Hello),
    Name == hello.

'structure has proper field names' :-
    exists(hello, Hello),
    setof(F,V^field(F,Hello,V),Fields),
    Fields == [lang,whom].

'populating fields of a struct value: dynamic accessor' :-
    exists(hello, H),
    field(whom, H, "Michael"),
    field(lang, H, en).

'populating fields of a struct value: static accessor' :-
    sample(_).

'wrong type on a field'(fail) :-
    exists(hello, H),
    field(whom, H, michael).  % should be a string, not atom
