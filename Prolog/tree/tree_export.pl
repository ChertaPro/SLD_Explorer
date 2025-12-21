%% =============================================================================
%% MÓDULO: tree_export.pl
%% Descripción: Exportación del árbol SLD a JSON para visualización web
%% =============================================================================

:- module(tree_export, [
    tree_to_json/2,
    export_tree_file/2,
    tree_to_json_string/2
]).

:- use_module(library(http/json)).

%% -----------------------------------------------------------------------------
%% tree_to_json(+Tree, -JSON)
%% Convierte el árbol SLD a estructura JSON
%% -----------------------------------------------------------------------------

tree_to_json(node(Goals, Subst, RuleApplied, Status, Children), JSON) :-
    % Convertir goals a string
    goals_to_string(Goals, GoalsStr),
    
    % Convertir sustitución a string
    subst_to_string(Subst, SubstStr),
    
    % Convertir regla aplicada a string
    rule_to_string(RuleApplied, RuleStr),
    
    % Convertir children recursivamente
    children_to_json(Children, ChildrenJSON),
    
    % Generar ID único para el nodo
    term_hash(node(Goals, Subst), NodeID),
    
    JSON = json([
        id = NodeID,
        goals = GoalsStr,
        substitution = SubstStr,
        rule = RuleStr,
        status = Status,
        children = ChildrenJSON
    ]).

%% -----------------------------------------------------------------------------
%% Conversión de elementos a string
%% -----------------------------------------------------------------------------

goals_to_string([], '[]').
goals_to_string(Goals, Str) :-
    Goals \= [],
    maplist(term_to_atom, Goals, Atoms),
    atomic_list_concat(Atoms, ', ', Str).

subst_to_string([], '{}').
subst_to_string(Subst, Str) :-
    Subst \= [],
    maplist(subst_pair_to_string, Subst, Pairs),
    atomic_list_concat(Pairs, ', ', Str).

subst_pair_to_string(Var=Value, Str) :-
    term_to_atom(Var, VarAtom),
    term_to_atom(Value, ValueAtom),
    atomic_list_concat([VarAtom, '=', ValueAtom], Str).

rule_to_string(query, 'Query') :- !.
rule_to_string(clause(Head, []), Str) :- !,
    term_to_atom(Head, HeadAtom),
    atomic_list_concat([HeadAtom, '.'], Str).
rule_to_string(clause(Head, Body), Str) :-
    term_to_atom(Head, HeadAtom),
    maplist(term_to_atom, Body, BodyAtoms),
    atomic_list_concat(BodyAtoms, ', ', BodyStr),
    atomic_list_concat([HeadAtom, ' :- ', BodyStr, '.'], Str).

children_to_json([], []).
children_to_json([H|T], [HJSON|TJSON]) :-
    tree_to_json(H, HJSON),
    children_to_json(T, TJSON).

%% -----------------------------------------------------------------------------
%% tree_to_json_string(+Tree, -JSONString)
%% Convierte árbol a string JSON (para output directo)
%% -----------------------------------------------------------------------------

tree_to_json_string(Tree, JSONString) :-
    tree_to_json(Tree, JSON),
    with_output_to(string(JSONString), json_write(current_output, JSON, [width(0)])).

%% -----------------------------------------------------------------------------
%% export_tree_file(+Tree, +Filename)
%% Exporta el árbol a un archivo JSON
%% -----------------------------------------------------------------------------

export_tree_file(Tree, Filename) :-
    tree_to_json(Tree, JSON),
    open(Filename, write, Stream),
    json_write(Stream, JSON, [width(0)]),
    close(Stream).

%% -----------------------------------------------------------------------------
%% Formato alternativo: Simple para debugging
%% -----------------------------------------------------------------------------

tree_to_simple_json(node(Goals, Subst, RuleApplied, Status, Children), JSON) :-
    length(Children, NumChildren),
    JSON = json([
        goals = Goals,
        substitution = Subst,
        rule = RuleApplied,
        status = Status,
        num_children = NumChildren
    ]).

%% -----------------------------------------------------------------------------
%% Exportación a formato Graphviz DOT (para visualización alternativa)
%% -----------------------------------------------------------------------------

tree_to_dot(Tree, DotString) :-
    with_output_to(string(DotString), tree_to_dot_output(Tree)).

tree_to_dot_output(Tree) :-
    format('digraph SLD {~n'),
    format('    node [shape=box, fontname="Courier"];~n'),
    format('    rankdir=TB;~n~n'),
    tree_to_dot_nodes(Tree, 0, _),
    format('}~n').

tree_to_dot_nodes(node(Goals, Subst, _, Status, Children), ID, NextID) :-
    % Crear nodo
    goals_to_string(Goals, GoalsStr),
    subst_to_string(Subst, SubstStr),
    status_to_color(Status, Color),
    format('    node~w [label="~w\\n~w", fillcolor=~w, style=filled];~n', 
           [ID, GoalsStr, SubstStr, Color]),
    
    % Crear nodos hijos y conexiones
    ChildID is ID + 1,
    tree_to_dot_children(Children, ID, ChildID, NextID).

tree_to_dot_children([], _, NextID, NextID).
tree_to_dot_children([Child|Rest], ParentID, CurrentID, NextID) :-
    tree_to_dot_nodes(Child, CurrentID, TempID),
    format('    node~w -> node~w;~n', [ParentID, CurrentID]),
    tree_to_dot_children(Rest, ParentID, TempID, NextID).

status_to_color(success, lightgreen).
status_to_color(failure, lightcoral).
status_to_color(has_solution, lightyellow).
status_to_color(pending, lightblue).
status_to_color(depth_limit, orange).