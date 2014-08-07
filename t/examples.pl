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

'structure has proper field names: field/2' :-
    exists(hello, Hello),
    setof(F,field(F,Hello),Fields),
    Fields == [lang,whom].

'structure has proper field names: field/3' :-
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

'extract fields and values with exists/3' :-
    sample(S),
    exists(N,S,Ps),
    N == hello,
    Ps == [lang-en, whom-"Michael"].

'extract values with exists/3' :-
    sample(S),
    exists(N, S, [lang-Lang]),
    N == hello,
    Lang == en.

'create specific struct with exists/3' :-
    exists(hello, S, [whom-"Johann",lang-de]),
    struct:lang(S,Lang),
    Lang == de,
    struct:whom(S,Whom),
    Whom == "Johann".

'create some struct with exists/3' :-
    exists(N, S, [whom-"Johann",lang-de]),
    N == hello,
    struct:lang(S,Lang),
    Lang == de,
    struct:whom(S,Whom),
    Whom == "Johann".

'check all struct fields with exists/3' :-
    sample(S),
    exists(N,S,[whom-"Michael",lang-en]),
    N == hello.

'query a field name with exists/3' :-
    sample(S),
    exists(_,S,[Field-en]), % which field holds the language?
    Field == lang.

'struct -> dictionary' :-
    sample(H),
    struct_dict(H,D),
    D.lang == en,
    D.whom == "Michael".

'dictionary -> struct' :-
    struct_dict(H,Tag{whom:"John",lang:es}),
    Tag == hello,
    struct:lang(H,Lang),
    Lang == es,
    struct:whom(H,Whom),
    Whom == "John".
