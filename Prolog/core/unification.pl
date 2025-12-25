%% =============================================================================
%% MÓDULO: unification.pl
%% Descripción: Motor de unificación de Robinson con occurs check
%% =============================================================================

:- module(unification, [
    unify/3,
    unify/4,
    apply_substitution/3,
    compose_substitutions/3,
    occurs_check/2
]).

%% -----------------------------------------------------------------------------
%% unify(+Term1, +Term2, -Substitution)
%% Unifica dos términos y devuelve la sustitución más general
%% Substitution es una lista de pares Var=Value
%% -----------------------------------------------------------------------------

unify(Term1, Term2, Subst) :-
    unify(Term1, Term2, [], Subst).

%% -----------------------------------------------------------------------------
%% unify(+Term1, +Term2, +SubstIn, -SubstOut)
%% Unifica dos términos dados una sustitución de entrada
%% -----------------------------------------------------------------------------

% Caso base: ambos términos son idénticos
unify(X, X, Subst, Subst) :- !.

% Variables
unify(X, Y, SubstIn, SubstOut) :-
    var(X), !,
    % Verificar si X ya tiene una vinculación en SubstIn
    (   member(X=Value, SubstIn) ->
        % X ya está vinculada, unificar su valor con Y
        unify(Value, Y, SubstIn, SubstOut)
    ;   % X no está vinculada
        (   occurs_check(X, Y) ->
            fail  % Occurs check fallido
        ;   var(Y), member(Y=YValue, SubstIn) ->
            % Y está vinculada, vincular X con el valor de Y
            SubstOut = [X=YValue|SubstIn]
        ;   % Agregar nueva vinculación
            SubstOut = [X=Y|SubstIn]
        )
    ).

unify(X, Y, SubstIn, SubstOut) :-
    var(Y), !,
    unify(Y, X, SubstIn, SubstOut).

% Términos compuestos
unify(Term1, Term2, SubstIn, SubstOut) :-
    compound(Term1),
    compound(Term2), !,
    Term1 =.. [Functor|Args1],
    Term2 =.. [Functor|Args2],  % Mismo functor
    length(Args1, N),
    length(Args2, N),            % Misma aridad
    unify_list(Args1, Args2, SubstIn, SubstOut).

% Listas (caso especial)
unify([H1|T1], [H2|T2], SubstIn, SubstOut) :- !,
    unify(H1, H2, SubstIn, SubstTemp),
    unify(T1, T2, SubstTemp, SubstOut).

unify([], [], Subst, Subst) :- !.

% Átomos y números
unify(X, Y, Subst, Subst) :-
    atomic(X),
    atomic(Y),
    X = Y, !.

unify(_, _, _, _) :- fail.

%% -----------------------------------------------------------------------------
%% unify_list(+List1, +List2, +SubstIn, -SubstOut)
%% Unifica listas de términos elemento a elemento
%% -----------------------------------------------------------------------------

unify_list([], [], Subst, Subst).
unify_list([H1|T1], [H2|T2], SubstIn, SubstOut) :-
    unify(H1, H2, SubstIn, SubstTemp),
    unify_list(T1, T2, SubstTemp, SubstOut).

%% -----------------------------------------------------------------------------
%% occurs_check(+Var, +Term)
%% Verifica si Var ocurre en Term (para evitar unificaciones cíclicas)
%% -----------------------------------------------------------------------------

occurs_check(Var, Term) :-
    var(Term), !,
    Var == Term.

occurs_check(Var, Term) :-
    compound(Term), !,
    Term =.. [_|Args],
    occurs_check_list(Var, Args).

occurs_check(Var, [H|T]) :- !,
    (occurs_check(Var, H) ; occurs_check(Var, T)).

occurs_check(_, _) :- fail.

occurs_check_list(Var, [H|T]) :-
    (occurs_check(Var, H) ; occurs_check_list(Var, T)).

%% -----------------------------------------------------------------------------
%% apply_substitution(+Term, +Subst, -Result)
%% Aplica una sustitución a un término
%% -----------------------------------------------------------------------------

apply_substitution(Var, Subst, Result) :-
    var(Var), !,
    (   member(Var=Value, Subst) ->
        apply_substitution(Value, Subst, Result)
    ;   Result = Var
    ).

apply_substitution(Term, Subst, Result) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    apply_substitution_list(Args, Subst, ResultArgs),
    Result =.. [Functor|ResultArgs].

apply_substitution([H|T], Subst, [RH|RT]) :- !,
    apply_substitution(H, Subst, RH),
    apply_substitution(T, Subst, RT).

apply_substitution([], _, []) :- !.

apply_substitution(Term, _, Term).  % Átomos, números

apply_substitution_list([], _, []).
apply_substitution_list([H|T], Subst, [RH|RT]) :-
    apply_substitution(H, Subst, RH),
    apply_substitution_list(T, Subst, RT).

%% -----------------------------------------------------------------------------
%% compose_substitutions(+Subst1, +Subst2, -Result)
%% Compone dos sustituciones: Result = Subst2 ∘ Subst1
%% Primero aplica Subst2 a los valores de Subst1, luego añade Subst2
%% -----------------------------------------------------------------------------

compose_substitutions(Subst1, Subst2, Result) :-
    % Aplicar Subst2 a todos los valores en Subst1
    apply_substitution_to_subst(Subst1, Subst2, UpdatedSubst1),
    % Agregar las nuevas vinculaciones de Subst2
    merge_substitutions(UpdatedSubst1, Subst2, Result).

apply_substitution_to_subst([], _, []).
apply_substitution_to_subst([Var=Val|Rest], Subst2, [Var=NewVal|RestResult]) :-
    apply_substitution(Val, Subst2, NewVal),
    apply_substitution_to_subst(Rest, Subst2, RestResult).

%% merge_substitutions(+Subst1, +Subst2, -Result)
%% Une dos sustituciones, evitando duplicados
merge_substitutions(Subst1, [], Subst1) :- !.
merge_substitutions(Subst1, [Var=Val|Rest], Result) :-
    % Si Var ya está en Subst1, no la agregamos de nuevo
    (   member(Var=_, Subst1) ->
        merge_substitutions(Subst1, Rest, Result)
    ;   merge_substitutions([Var=Val|Subst1], Rest, Result)
    ).