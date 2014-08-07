:- module(struct, [ op(1160,fx,(structure))
                  , (structure)/1

                  % reflection
                  , current_structure/1
                  , structure_property/2
                  , field_property/3

                  % relations
                  , defaults/1
                  , exists/2
                  , exists/3
                  , field/3
                  , in_db/1
                  , is_struct/1
                  , struct_dict/2
                  ]).

:- use_module(library(lambda)).
:- use_module(library(pairs), [pairs_keys_values/3]).

% define struct types for library(error)
:- multifile error:has_type/2.
error:has_type(struct, Struct) :-
    is_struct(Struct).
error:has_type(struct(Name), Struct) :-
    atom(Name),
    is_struct(Struct),
    struct_name(Struct,Name).


%% defaults(:Struct:struct) is det.
%
%  True if uninstantiated field values in Struct can be unified with the
%  default value for that field, if one exists. This is typically used
%  after declaring everything one knows about a struct but before using
%  it.  For example,
%
%      exists(person, P),
%      struct:name(P, jason),  % we know his name
%      defaults(P).            % use default values for other fields
defaults(Struct) :-
    must_be(nonvar,Struct),
    struct_name(Struct, StructName),
    bagof(F-D,field_property(StructName,F,default(D)),Defaults),
    foreach( member(F-D,Defaults)
           , defaults_(F,Struct,D)
           ).

defaults_(FieldName,Struct,DefaultValue) :-
    field(FieldName, Struct, Value),
    ( var(Value) -> field(FieldName,Struct,DefaultValue); true ).


%% exists(+Name:atom, -Struct:struct) is det.
%% exists(-Name:atom, +Struct:struct) is det.
%
%  True if Struct is a structure named Name. Raises a signal if there is
%  no structure named Name.
:- multifile exists/2.


%% exists(?Name:atom, +Struct:struct, +FieldValues:pairs) is semidet.
%% exists(?Name:atom, +Struct:struct, -FieldValues:pairs) is semidet.
%% exists(+Name:atom, -Struct:struct, +FieldValues:pairs) is semidet.
%% exists(-Name:atom, -Struct:struct, +FieldValues:pairs) is nondet.
%
%  True if a Struct named Name has field-value pairs as described by
%  FieldValues.
exists(Name,Struct,FieldValues) :-
    when( ground(Name);nonvar(Struct), exists(Name,Struct) ),

    ( is_list(FieldValues) ->  % declaring struct based on fields
        maplist(exists_mapper(Struct),FieldValues)
    ; true -> % extracting fields from struct
        must_be(ground, Name),  % otherwise, exists/3 mode was bad
        setof(F,structure_property(Name,field(F)),Fields),
        maplist(\F^V^(field(F,Struct,V)),Fields,Values),
        pairs_keys_values(FieldValues,Fields,Values)
    ).

exists_mapper(Struct,Field-Value) :-
    field(Field,Struct,Value).


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
%      * default(V) - optional default value
%      * position(N) - =|arg(N,_,_)|= for this field
%      * type(T) - field's type. =|any|= if none was specified.
:- multifile field_property/3.


struct_name(Struct,StructName) :-
    must_be(nonvar,Struct),
    functor(Struct,StructName,_).


%% struct_dict(+Struct:struct, -Dict:dict) is det.
%% struct_dict(-Struct:struct, +Dict:dict) is nondet.
%
%  True if Struct and Dict represent the same data. The name of the
%  Struct type is used as the tag of the Dict.
struct_dict(Struct,Dict) :-
    ( nonvar(Dict) ->
        dict_pairs(Dict,Name,Pairs),
        exists(Name,Struct,Pairs)
    ; must_be(nonvar,Struct) ->
        exists(Name,Struct,Pairs),
        dict_pairs(Dict,Name,Pairs)
    ).


%% in_db(?Struct:struct)
%
%  True if Struct is found in the database. Iterates solutions on
%  backtracking.
%
%  This is temporarily an alias for call/1 with automatic
%  conversion from dicts. It may eventually consider a broader
%  definition of "db".
in_db(Struct0) :-
    ( is_dict(Struct0) -> struct_dict(Struct,Struct0); Struct=Struct0 ),
    call(Struct).

%% is_struct(+Struct:struct) is semidet.
%
%  True if Struct is a valid struct value. Validity includes
%  checking name, arity and field types.
is_struct(Struct) :-
    compound(Struct),
    \+ \+ exists(_,Struct).  % don't add field type constraints


/******** macro code below here ***********/

structure(Definition) :-
    throw(struct("structure macro not expanded",Definition)).


% define macros in a separate predicate to ease testing
macro(term,Dict,Fact) :-
    is_dict(Dict),
    struct_dict(Fact, Dict).
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

    [ error:has_type(StructName,Empty) :-
          error:has_type(struct(StructName),Empty)
    ],

    per_arg(Args,1,StructName,Arity).


per_arg([],_,_,_) -->
    [].
per_arg([Arg|Args],Position0,StructName,Arity) -->
    { once( (Arg=(FieldName:Type=Default))
          ; (Arg=(FieldName:Type))
          ; (Arg=(FieldName=Default),Type=any)
          ; (Arg=(FieldName),Type=any)
          )
    },
    [ struct:structure_property(StructName,field(FieldName)) ],
    [ struct:field_property(StructName,FieldName,type(Type)) ],
    [ struct:field_property(StructName,FieldName,position(Position0)) ],
    ( { nonvar(Default) } ->
        [ struct:field_property(StructName,FieldName,default(Default)) ]
    ; []
    ),

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
