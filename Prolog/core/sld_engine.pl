%% =============================================================================
%% MÓDULO: sld_engine.pl
%% Descripción: Motor de resolución SLD
%% =============================================================================

:- module(sld_engine, [
    sld_resolution/3,
    sld_resolution_with_depth/4
]).

:- use_module(unification).

%% -----------------------------------------------------------------------------
%% Estructura del árbol SLD:
%% node(
%%     GoalList,        % Lista de goals actuales [goal1, goal2, ...]
%%     Substitution,    % Sustitución acumulada [X=a, Y=b, ...]
%%     RuleApplied,     % Cláusula aplicada: clause(Head, Body) o 'query'
%%     Status,          % success | failure | pending
%%     Children         % Lista de nodos hijos
%% )
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% sld_resolution(+Program, +Query, -Tree)
%% Genera el árbol SLD completo para una consulta
%% Program: lista de cláusulas clause(Head, Body)
%% Query: lista de goals
%% Tree: árbol SLD completo
%% -----------------------------------------------------------------------------

sld_resolution(Program, Query, Tree) :-
    sld_resolution_with_depth(Program, Query, 100, Tree).

%% -----------------------------------------------------------------------------
%% sld_resolution_with_depth(+Program, +Query, +MaxDepth, -Tree)
%% Versión con límite de profundidad para evitar bucles infinitos
%% -----------------------------------------------------------------------------

sld_resolution_with_depth(Program, Query, MaxDepth, Tree) :-
    MaxDepth > 0,
    rename_variables(Query, RenamedQuery),
    expand_sld_tree(Program, RenamedQuery, [], 'query', MaxDepth, Tree).

%% -----------------------------------------------------------------------------
%% expand_sld_tree(+Program, +Goals, +Subst, +RuleApplied, +Depth, -Node)
%% Expande un nodo del árbol SLD
%% -----------------------------------------------------------------------------

% Caso éxito: no quedan goals por resolver
expand_sld_tree(_, [], Subst, RuleApplied, _, Node) :- !,
    Node = node([], Subst, RuleApplied, success, []).

% Caso límite de profundidad alcanzado
expand_sld_tree(_, Goals, Subst, RuleApplied, 0, Node) :- !,
    Node = node(Goals, Subst, RuleApplied, depth_limit, []).

% Caso general: expandir el primer goal
expand_sld_tree(Program, [Goal|RestGoals], Subst, RuleApplied, Depth, Node) :-
    Depth > 0,
    findall(
        Child,
        (
            % Encontrar una cláusula unificable
            member(Clause, Program),
            Clause = clause(Head, Body),
            rename_clause(Clause, RenamedClause),
            RenamedClause = clause(RenamedHead, RenamedBody),
            
            % Intentar unificar
            unify(Goal, RenamedHead, Subst, NewSubst),
            
            % Aplicar sustitución a los goals restantes
            apply_substitution(RestGoals, NewSubst, SubstRestGoals),
            apply_substitution(RenamedBody, NewSubst, SubstBody),
            
            % Nueva lista de goals
            append(SubstBody, SubstRestGoals, NewGoals),
            
            % Expandir recursivamente
            NewDepth is Depth - 1,
            expand_sld_tree(Program, NewGoals, NewSubst, RenamedClause, NewDepth, Child)
        ),
        Children
    ),
    
    % Determinar status del nodo
    (   Children = [] ->
        Status = failure
    ;   member(node(_, _, _, success, _), Children) ->
        Status = has_solution
    ;   Status = pending
    ),
    
    Node = node([Goal|RestGoals], Subst, RuleApplied, Status, Children).

%% -----------------------------------------------------------------------------
%% rename_variables(+Term, -RenamedTerm)
%% Renombra todas las variables en un término para evitar colisiones
%% -----------------------------------------------------------------------------

rename_variables(Term, RenamedTerm) :-
    copy_term(Term, RenamedTerm).

rename_clause(clause(Head, Body), clause(RenamedHead, RenamedBody)) :-
    copy_term(clause(Head, Body), clause(RenamedHead, RenamedBody)).

%% -----------------------------------------------------------------------------
%% Predicados auxiliares para análisis del árbol
%% -----------------------------------------------------------------------------

% Contar nodos en el árbol
count_nodes(node(_, _, _, _, Children), Count) :-
    count_nodes_list(Children, ChildCount),
    Count is 1 + ChildCount.

count_nodes_list([], 0).
count_nodes_list([H|T], Count) :-
    count_nodes(H, HCount),
    count_nodes_list(T, TCount),
    Count is HCount + TCount.

% Obtener todas las soluciones (nodos de éxito)
find_all_solutions(node(_, Subst, _, success, _), [Subst]).
find_all_solutions(node(_, _, _, _, Children), Solutions) :-
    find_all_solutions_list(Children, Solutions).

find_all_solutions_list([], []).
find_all_solutions_list([H|T], Solutions) :-
    find_all_solutions(H, HSolutions),
    find_all_solutions_list(T, TSolutions),
    append(HSolutions, TSolutions, Solutions).

% Obtener profundidad máxima del árbol
tree_depth(node(_, _, _, _, []), 1).
tree_depth(node(_, _, _, _, Children), Depth) :-
    Children \= [],
    max_depth_list(Children, ChildMaxDepth),
    Depth is 1 + ChildMaxDepth.

max_depth_list([H], Depth) :- tree_depth(H, Depth).
max_depth_list([H|T], MaxDepth) :-
    tree_depth(H, HDepth),
    max_depth_list(T, TDepth),
    MaxDepth is max(HDepth, TDepth).