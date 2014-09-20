:- module(struct_macros, []).


% define macros in a separate predicate to ease testing
macro(term,Dict,Fact) :-
    is_dict(Dict),
    struct:struct_dict(Fact, Dict).
macro(term,(:-structure(Definition)),Terms) :-
    functor(Definition,Name,Arity),
    ( struct:current_structure(Name) ->
          throw(struct("duplicate structure",Name))
    ; true ),
    ( Arity == 0 -> throw(struct("invalid arity",Name,Arity)); true ),
    Definition =.. [_|Args],
    phrase(per_struct(Args,Name,Arity),Terms).


per_struct(Args,StructName,Arity) -->
    [ struct:structure_property(StructName,arity(Arity)) ],

    { functor(Empty,StructName,Arity) },
    [ struct:exists(StructName,Empty) :-
          struct_macros:constrain_fields(Empty)
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
    modifiers(StructName,Arity,FieldName,Position0),

    { succ(Position0,Position) },
    per_arg(Args,Position,StructName,Arity).


accessors(StructName,Arity,FieldName,ArgPosition) -->
    { functor(Struct,StructName,Arity) },

    % FieldName/1 semidet accessor
    { Accessor1 =.. [FieldName,Struct] },
    [ :- multifile struct:FieldName/1 ],
    [ struct:Accessor1 ],
    [ struct:field(FieldName,Struct) ],

    % FieldName/2 accessor predicate
    { Accessor2 =.. [FieldName,Struct,Value] },
    { arg(ArgPosition,Struct,Value) },
    [ :- multifile struct:FieldName/2 ],
    [ struct:Accessor2 ],
    [ struct:field(FieldName,Struct,Value) ].

modifiers(StructName,Arity,FieldName,ArgPosition) -->
    modifier_3(StructName,Arity,FieldName,ArgPosition),
    modifier_4(StructName,Arity,FieldName,ArgPosition),
    modifier_5(StructName,Arity,FieldName,ArgPosition).

modifier_3(StructName,Arity,FieldName,ArgPosition) -->
    % FieldName/3 modifier predicate
    { functor(Struct0,StructName,Arity) },
    { functor(Struct1,StructName,Arity) },
    { same_but(Struct0, Struct1, ArgPosition) },
    { arg(ArgPosition,Struct1,Val1) },
    { Modifier3 =.. [FieldName, Val1, Struct0, Struct1] },
    [ :- multifile FieldName/3 ],
    [ struct:Modifier3 ].

modifier_4(StructName,Arity,FieldName,ArgPosition) -->
    % FieldName/4 modifier predicate
    { functor(Struct0,StructName,Arity) },
    { functor(Struct1,StructName,Arity) },
    { same_but(Struct0, Struct1, ArgPosition )},
    { arg(ArgPosition,Struct0,Val0) },
    { arg(ArgPosition,Struct1,Val1) },
    { Modifier4 =.. [FieldName,Val0,Val1,Struct0,Struct1] },
    [ :- multifile FieldName/4 ],
    [ struct:Modifier4 ].

modifier_5(StructName,Arity,FieldName,ArgPosition) -->
    % field/5 modifier predicate
    { functor(Struct0,StructName,Arity) },
    { functor(Struct1,StructName,Arity) },
    { same_but(Struct0, Struct1, ArgPosition )},
    { arg(ArgPosition,Struct0,Val0) },
    { arg(ArgPosition,Struct1,Val1) },
    [ struct:field(FieldName,Val0,Val1,Struct0,Struct1) ].


% Struct0 and Struct1 share all arguments except number N
same_but(Struct0, Struct1, N) :-
    Struct0 =.. List0,
    Struct1 =.. List1,
    length(List0, Size),
    Max is Size - 1,
    setof(I,between(0,Max,I),Indexes),
    maplist(same_but_(N),Indexes,List0,List1).

same_but_(N,I,Arg0,Arg1) :-
    ( N=I -> true; Arg0=Arg1 ).


constrain_fields(Struct) :-
    struct:struct_name(Struct,StructName),
    bagof(N-T,struct:field_property(StructName,N,type(T)),Types),
    maplist(constrain_field(Struct), Types).

constrain_field(Struct,Name-Type) :-
    ( Type = any ->
        true % no constraint needed
    ; true ->
        struct:field(Name,Struct,Var),
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
