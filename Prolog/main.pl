%% =============================================================================
%% MÓDULO: main.pl
%% Descripción: Punto de entrada principal del SLD Explorer
%% =============================================================================

:- use_module(unification).
:- use_module(sld_engine).
:- use_module(tree_export).

%% -----------------------------------------------------------------------------
%% API Principal
%% -----------------------------------------------------------------------------

%% explore_query(+ProgramFile, +Query)
%% Carga un programa y explora una consulta
explore_query(ProgramFile, Query) :-
    load_program(ProgramFile, Program),
    format('~n=== SLD Explorer ===~n'),
    format('Program: ~w~n', [ProgramFile]),
    format('Query: ~w~n~n', [Query]),
    
    sld_resolution(Program, Query, Tree),
    
    % Mostrar estadísticas
    print_tree_stats(Tree),
    
    % Exportar a JSON
    atom_concat(ProgramFile, '.json', JSONFile),
    export_tree_file(Tree, JSONFile),
    format('~nTree exported to: ~w~n', [JSONFile]).

%% explore_query_interactive
%% Modo interactivo
explore_query_interactive :-
    format('~n=== SLD Explorer - Interactive Mode ===~n'),
    format('Enter program file: '),
    read(ProgramFile),
    format('Enter query (as list): '),
    read(Query),
    explore_query(ProgramFile, Query).

%% -----------------------------------------------------------------------------
%% Carga de programas
%% -----------------------------------------------------------------------------

load_program(File, Program) :-
    see(File),
    read_clauses(Clauses),
    seen,
    convert_to_internal(Clauses, Program).

read_clauses(Clauses) :-
    read(Term),
    (   Term = end_of_file ->
        Clauses = []
    ;   Clauses = [Term|Rest],
        read_clauses(Rest)
    ).

% Convierte cláusulas Prolog estándar a representación interna
convert_to_internal([], []).
convert_to_internal([Clause|Rest], [Internal|RestInternal]) :-
    clause_to_internal(Clause, Internal),
    convert_to_internal(Rest, RestInternal).

clause_to_internal((Head :- Body), clause(Head, BodyList)) :- !,
    body_to_list(Body, BodyList).
clause_to_internal(Head, clause(Head, [])).

body_to_list((A, B), [A|Rest]) :- !,
    body_to_list(B, Rest).
body_to_list(A, [A]).

%% -----------------------------------------------------------------------------
%% Utilidades de visualización
%% -----------------------------------------------------------------------------

print_tree_stats(Tree) :-
    count_nodes(Tree, NumNodes),
    tree_depth(Tree, Depth),
    find_all_solutions(Tree, Solutions),
    length(Solutions, NumSolutions),
    
    format('=== Tree Statistics ===~n'),
    format('Total nodes: ~w~n', [NumNodes]),
    format('Tree depth: ~w~n', [Depth]),
    format('Solutions found: ~w~n', [NumSolutions]),
    format('~n=== Solutions ===~n'),
    print_solutions(Solutions, 1).

print_solutions([], _).
print_solutions([Subst|Rest], N) :-
    format('Solution ~w: ', [N]),
    subst_to_string(Subst, SubstStr),
    format('~w~n', [SubstStr]),
    N1 is N + 1,
    print_solutions(Rest, N1).

print_tree(Tree) :-
    print_tree(Tree, 0).

print_tree(node(Goals, Subst, Rule, Status, Children), Indent) :-
    indent(Indent),
    goals_to_string(Goals, GoalsStr),
    subst_to_string(Subst, SubstStr),
    rule_to_string(Rule, RuleStr),
    format('[~w] Goals: ~w | Subst: ~w | Rule: ~w~n', 
           [Status, GoalsStr, SubstStr, RuleStr]),
    Indent1 is Indent + 2,
    print_tree_children(Children, Indent1).

print_tree_children([], _).
print_tree_children([H|T], Indent) :-
    print_tree(H, Indent),
    print_tree_children(T, Indent).

indent(0) :- !.
indent(N) :-
    N > 0,
    write('  '),
    N1 is N - 1,
    indent(N1).

%% =============================================================================
%% EJEMPLOS DE USO
%% =============================================================================

%% Ejemplo 1: Programa simple de familia
example_family :-
    Program = [
        clause(parent(tom, bob), []),
        clause(parent(tom, liz), []),
        clause(parent(bob, ann), []),
        clause(parent(bob, pat), []),
        clause(parent(pat, jim), []),
        clause(grandparent(X, Z), [parent(X, Y), parent(Y, Z)])
    ],
    Query = [grandparent(tom, W)],
    
    format('~n=== Example: Family Relations ===~n'),
    sld_resolution(Program, Query, Tree),
    print_tree_stats(Tree),
    format('~n=== Tree Structure ===~n'),
    print_tree(Tree).

%% Ejemplo 2: Listas - member/2
example_member :-
    Program = [
        clause(member(X, [X|_]), []),
        clause(member(X, [_|T]), [member(X, T)])
    ],
    Query = [member(2, [1, 2, 3])],
    
    format('~n=== Example: Member ===~n'),
    sld_resolution(Program, Query, Tree),
    print_tree_stats(Tree),
    format('~n=== Tree Structure ===~n'),
    print_tree(Tree).

%% Ejemplo 3: Append
example_append :-
    Program = [
        clause(append([], L, L), []),
        clause(append([H|T1], L2, [H|T3]), [append(T1, L2, T3)])
    ],
    Query = [append([1, 2], [3, 4], Z)],
    
    format('~n=== Example: Append ===~n'),
    sld_resolution(Program, Query, Tree),
    print_tree_stats(Tree),
    format('~n=== Tree Structure ===~n'),
    print_tree(Tree).

%% Ejemplo 4: Backtracking
example_backtracking :-
    Program = [
        clause(color(red), []),
        clause(color(green), []),
        clause(color(blue), []),
        clause(likes(alice, red), []),
        clause(likes(bob, blue), []),
        clause(match(X, C), [color(C), likes(X, C)])
    ],
    Query = [match(alice, Color)],
    
    format('~n=== Example: Backtracking ===~n'),
    sld_resolution(Program, Query, Tree),
    print_tree_stats(Tree),
    format('~n=== Tree Structure ===~n'),
    print_tree(Tree).

%% Ejemplo 5: Fallo (no hay solución)
example_failure :-
    Program = [
        clause(p(a), []),
        clause(q(b), [])
    ],
    Query = [p(X), q(X)],
    
    format('~n=== Example: Failure (No Solution) ===~n'),
    sld_resolution(Program, Query, Tree),
    print_tree_stats(Tree),
    format('~n=== Tree Structure ===~n'),
    print_tree(Tree).

%% Ejecutar todos los ejemplos
run_examples :-
    example_family,
    format('~n~n'),
    example_member,
    format('~n~n'),
    example_append,
    format('~n~n'),
    example_backtracking,
    format('~n~n'),
    example_failure.

%% =============================================================================
%% COMANDOS RÁPIDOS
%% =============================================================================

:- format('~n=== SLD Explorer Loaded ===~n').
:- format('Quick commands:~n').
:- format('  run_examples.           - Run all built-in examples~n').
:- format('  example_family.         - Family relations example~n').
:- format('  example_member.         - List member example~n').
:- format('  example_append.         - List append example~n').
:- format('  example_backtracking.   - Backtracking example~n').
:- format('  explore_query(File, Q). - Explore custom query~n').
:- format('~n').