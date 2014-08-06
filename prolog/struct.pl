:- module(struct, [ op(1160,fx,(structure))
                  , (structure)/1

                  % reflection
                  , current_structure/1
                  , structure_property/2
                  , field_property/3

                  % relations
                  , exists/2
                  , field/3
                  ]).



%% exists(+Name:atom, -Struct:struct) is det.
%% exists(-Name:atom, +Struct:struct) is det.
%
%  True if Struct is a structure named Name. Raises a signal if there is
%  no structure named Name.
:- multifile exists/2.


%% field(+FieldName:atom,+Struct:struct,-Value) is det.
%% field(+FieldName:atom,-Struct:struct,+Value) is multi.
%% field(-FieldName:atom,+Struct:struct,-Value) is multi.
%
%  True if Struct has a field named FieldName where that field has
%  Value.
:- multifile field/3.


%% current_structure(+Name:atom) is semidet.
%% current_structure(-Name:atom) is nondet.
%
%  True if a structure named Name exists.
current_structure(Name) :-
    exists(Name,_).


%% structure_property(?StructName:atom,?Property)
%
%  True if a structure named StructName has a Property. Property is one
%  of:
%
%      * arity(N) - number of fields N
%      * field(F) - a field named F
:- multifile structure_property/2.


%% field_property(?StructName:atom,?FieldName:atom,?Property)
%
%  True if a structure named StructName has a field named FieldName and
%  that field has a Property.  Property is one of:
%
%      * type(T) - field's type. =|any|= if none was specified.
:- multifile field_property/3.


structure(Definition) :-
    throw(struct("structure macro not expanded",Definition)).


struct_name(Struct,StructName) :-
    must_be(nonvar,Struct),
    functor(Struct,StructName,_).


% define macros in a separate predicate to ease testing
macro(term,(:-structure(Definition)),Terms) :-
    functor(Definition,Name,Arity),
    ( current_structure(Name)-> throw(struct("duplicate structure",Name)); true ),
    ( Arity == 0 -> throw(struct("invalid arity",Name,Arity)); true ),
    Definition =.. [_|Args],
    phrase(expansion(Args,Name,Arity),Terms).


expansion(Args,StructName,Arity) -->
    [ struct:structure_property(StructName,arity(Arity)) ],

    { functor(Empty,StructName,Arity) },
    [ struct:exists(StructName,Empty) :-
          struct:constrain_fields(Empty)
    ],

    per_arg(Args,1,StructName,Arity).


per_arg([],_,_,_) -->
    [].
per_arg([Arg|Args],Position0,StructName,Arity) -->
    { once( Arg=FieldName:Type
          ; (FieldName=Arg, Type=any)
          )
    },
    [ struct:structure_property(StructName,field(FieldName)) ],
    [ struct:field_property(StructName,FieldName,type(Type)) ],
    [ struct:field_property(StructName,FieldName,position(Position0)) ],

    accessors(StructName,Arity,FieldName,Position0),

    { succ(Position0,Position) },
    per_arg(Args,Position,StructName,Arity).


accessors(StructName,Arity,FieldName,ArgPosition) -->
    { Head =.. [FieldName,Struct,Value] },
    { functor(Struct,StructName,Arity) },
    { arg(ArgPosition,Struct,Value) },
    [ :- multifile struct:FieldName/2 ],
    [ struct:Head ],
    [ struct:field(FieldName,Struct,Value) ].


constrain_fields(Struct) :-
    struct_name(Struct,StructName),
    bagof(N-T,field_property(StructName,N,type(T)),Types),
    maplist(constrain_field(Struct), Types).

constrain_field(Struct,Name-Type) :-
    ( Type = any ->
        true % no constraint needed
    ; true ->
        field(Name,Struct,Var),
        when(ground(Var),check_type(Struct,Name,Type,Var))
    ).

check_type(_Struct,_Name,Type,Var) :-
    error:is_of_type(Type, Var).


% true if module being loaded wants our macros expanded
wants_struct :-
    prolog_load_context(module, Module),
    Module \== struct, % don't expand ourselves
    current_predicate(Module:(structure)/1),  % prevent autoloading
    predicate_property(Module:structure(_),imported_from(struct)).


% expand term macros
user:term_expansion(Old,New) :-
    wants_struct,
    macro(term, Old, New).
