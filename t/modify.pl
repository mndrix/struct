:- use_module(library(struct)).

:- structure hello(whom:string, lang:atom).

sample(X) :-
    exists(hello, X),
    struct:whom(X,"Michael"),
    struct:lang(X,en).

:- use_module(library(tap)).


'full field modify: static' :-
    sample(X),
    struct:lang(en,de,X,Y),
    struct_dict(Y, Dict),
    Dict == hello{lang: de, whom: "Michael"}.

'full field modify: dynamic' :-
    sample(X),
    field(lang, en, de, X, Y),
    struct_dict(Y, Dict),
    Dict == hello{lang: de, whom: "Michael"}.

'quick field modify: static' :-
    sample(X),
    struct:lang(de,X,Y),
    struct_dict(Y, Dict),
    Dict == hello{lang: de, whom: "Michael"}.

'quick field modify: dynamic' :-
    sample(X),
    field(lang,de,X,Y),
    struct_dict(Y, Dict),
    Dict == hello{lang: de, whom: "Michael"}.
