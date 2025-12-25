%% =============================================================================
%% MÓDULO: sld_engine.pl
%% Descripción: Motor de resolución SLD (CORREGIDO - Sustituciones Visibles)
%% =============================================================================

:- module(sld_engine, [
    sld_resolution/3,
    sld_resolution_with_depth/4,
    count_nodes/2,
    tree_depth/2,
    find_all_solutions/2
]).

:- use_module(unification).

%% -----------------------------------------------------------------------------
%% Estructura del árbol SLD:
%% node(
%%     GoalList,        % Lista de goals actuales [goal1, goal2, ...]
%%     Substitution,    % Sustitución ACUMULADA hasta este nodo
%%     NewSubst,        % Sustitución NUEVA aplicada en este paso
%%     RuleApplied,     % Cláusula aplicada: clause(Head, Body) o 'query'
%%     Status,          % success | failure | pending | has_solution
%%     Children         % Lista de nodos hijos
%% )
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% sld_resolution(+Program, +Query, -Tree)
%% Genera el árbol SLD completo para una consulta
%% -----------------------------------------------------------------------------

sld_resolution(Program, Query, Tree) :-
    sld_resolution_with_depth(Program, Query, 100, Tree).

%% -----------------------------------------------------------------------------
%% sld_resolution_with_depth(+Program, +Query, +MaxDepth, -Tree)
%% Versión con límite de profundidad para evitar bucles infinitos
%% -----------------------------------------------------------------------------

sld_resolution_with_depth(Program, Query, MaxDepth, Tree) :-
    MaxDepth > 0,
    % NO renombramos la consulta inicial - mantenemos las variables originales
    expand_sld_tree(Program, Query, [], [], 'query', MaxDepth, Tree).

%% -----------------------------------------------------------------------------
%% expand_sld_tree(+Program, +Goals, +AccSubst, +NewSubst, +Rule, +Depth, -Node)
%% Expande un nodo del árbol SLD
%% AccSubst: sustitución acumulada desde la raíz
%% NewSubst: sustitución aplicada en ESTE paso específico
%% -----------------------------------------------------------------------------

% Caso éxito: no quedan goals por resolver
expand_sld_tree(_, [], AccSubst, NewSubst, RuleApplied, _, Node) :- !,
    Node = node([], AccSubst, NewSubst, RuleApplied, success, []).

% Caso límite de profundidad alcanzado
expand_sld_tree(_, Goals, AccSubst, NewSubst, RuleApplied, 0, Node) :- !,
    Node = node(Goals, AccSubst, NewSubst, RuleApplied, depth_limit, []).

% Caso general: expandir el primer goal
expand_sld_tree(Program, [Goal|RestGoals], AccSubst, NewSubst, RuleApplied, Depth, Node) :-
    Depth > 0,
    findall(
        child_info(ChildNewSubst, Child),
        (
            % Encontrar una cláusula unificable
            member(clause(ClauseHead, ClauseBody), Program),
            % IMPORTANTE: Renombramos SOLO la cláusula, no la consulta
            rename_clause(clause(ClauseHead, ClauseBody), RenamedClause),
            RenamedClause = clause(RenamedHead, RenamedBody),
            
            % Aplicar la sustitución acumulada al goal actual antes de unificar
            unification:apply_substitution(Goal, AccSubst, SubstGoal),
            
            % Intentar unificar el goal sustituido con la cabeza de la cláusula
            % unify/3 retorna SOLO las nuevas vinculaciones
            unification:unify(SubstGoal, RenamedHead, ChildNewSubst),
            
            % Componer con la sustitución acumulada
            unification:compose_substitutions(AccSubst, ChildNewSubst, ComposedSubst),
            
            % Aplicar sustitución compuesta a los goals restantes
            unification:apply_substitution(RestGoals, ComposedSubst, SubstRestGoals),
            unification:apply_substitution(RenamedBody, ComposedSubst, SubstBody),
            
            % Nueva lista de goals
            append(SubstBody, SubstRestGoals, NewGoals),
            
            % Expandir recursivamente con la sustitución compuesta
            NewDepth is Depth - 1,
            expand_sld_tree(Program, NewGoals, ComposedSubst, ChildNewSubst, RenamedClause, NewDepth, Child)
        ),
        ChildrenInfo
    ),
    
    % Extraer los nodos hijos
    findall(Child, member(child_info(_, Child), ChildrenInfo), Children),
    
    % Determinar status del nodo
    (   Children = [] ->
        Status = failure
    ;   member(node(_, _, _, _, success, _), Children) ->
        Status = has_solution
    ;   Status = pending
    ),
    
    % Crear el nodo con la sustitución acumulada y la nueva
    Node = node([Goal|RestGoals], AccSubst, NewSubst, RuleApplied, Status, Children).

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
count_nodes(node(_, _, _, _, _, Children), Count) :-
    count_nodes_list(Children, ChildCount),
    Count is 1 + ChildCount.

count_nodes_list([], 0).
count_nodes_list([H|T], Count) :-
    count_nodes(H, HCount),
    count_nodes_list(T, TCount),
    Count is HCount + TCount.

% Obtener todas las soluciones (nodos de éxito)
find_all_solutions(node(_, Subst, _, _, success, _), [Subst]).
find_all_solutions(node(_, _, _, _, _, Children), Solutions) :-
    find_all_solutions_list(Children, Solutions).

find_all_solutions_list([], []).
find_all_solutions_list([H|T], Solutions) :-
    find_all_solutions(H, HSolutions),
    find_all_solutions_list(T, TSolutions),
    append(HSolutions, TSolutions, Solutions).

% Obtener profundidad máxima del árbol
tree_depth(node(_, _, _, _, _, []), 1).
tree_depth(node(_, _, _, _, _, Children), Depth) :-
    Children \= [],
    max_depth_list(Children, ChildMaxDepth),
    Depth is 1 + ChildMaxDepth.

max_depth_list([H], Depth) :- tree_depth(H, Depth).
max_depth_list([H|T], MaxDepth) :-
    tree_depth(H, HDepth),
    max_depth_list(T, TDepth),
    MaxDepth is max(HDepth, TDepth).