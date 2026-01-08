"""
test_parser.py - Tests del parser

Tests esenciales:
- Parsing de hechos y reglas
- Parsing de consultas
- Parsing de listas
- Manejo de errores
"""

import pytest
from src.parser import (
    parse, parse_query, ParseError,
    Variable, Atom, Number, Compound
)


class TestFactParsing:
    """Tests de parsing de hechos"""
    
    def test_simple_fact(self):
        """Hecho simple: padre(juan, maria)."""
        program = parse("padre(juan, maria).")
        
        assert len(program.clauses) == 1
        clause = program.clauses[0]
        assert clause.is_fact()
        assert clause.head.functor == "padre"
        assert clause.head.arity == 2
    
    def test_multiple_facts(self):
        """Múltiples hechos"""
        code = """
        padre(juan, maria).
        madre(ana, maria).
        padre(pedro, juan).
        """
        program = parse(code)
        
        assert len(program.clauses) == 3
        assert all(c.is_fact() for c in program.clauses)
    
    def test_fact_with_numbers(self):
        """Hecho con números: edad(juan, 30)."""
        program = parse("edad(juan, 30).")
        
        clause = program.clauses[0]
        assert isinstance(clause.head.args[1], Number)
        assert clause.head.args[1].value == 30


class TestRuleParsing:
    """Tests de parsing de reglas"""
    
    def test_simple_rule(self):
        """Regla simple: abuelo(X, Z) :- padre(X, Y), padre(Y, Z)."""
        program = parse("abuelo(X, Z) :- padre(X, Y), padre(Y, Z).")
        
        assert len(program.clauses) == 1
        clause = program.clauses[0]
        assert clause.is_rule()
        assert len(clause.body) == 2
    
    def test_rule_with_single_goal(self):
        """Regla con un solo goal"""
        program = parse("persona(X) :- humano(X).")
        
        clause = program.clauses[0]
        assert clause.is_rule()
        assert len(clause.body) == 1
    
    def test_rule_with_multiple_goals(self):
        """Regla con múltiples goals"""
        program = parse("ancestro(X, Z) :- padre(X, Y), padre(Y, Z), padre(Z, W).")
        
        clause = program.clauses[0]
        assert len(clause.body) == 3


class TestQueryParsing:
    """Tests de parsing de consultas"""
    
    def test_simple_query(self):
        """Consulta simple: ?- padre(juan, X)."""
        query = parse_query("?- padre(juan, X).")
        
        assert len(query.goals) == 1
        goal = query.goals[0]
        assert goal.functor == "padre"
        assert isinstance(goal.args[1], Variable)
    
    def test_query_with_multiple_goals(self):
        """Consulta con múltiples goals"""
        query = parse_query("?- padre(X, Y), madre(Z, Y).")
        
        assert len(query.goals) == 2
    
    def test_query_with_ground_terms(self):
        """Consulta solo con términos ground"""
        query = parse_query("?- padre(juan, maria).")
        
        goal = query.goals[0]
        assert all(isinstance(arg, Atom) for arg in goal.args)


class TestListParsing:
    """Tests de parsing de listas"""
    
    def test_empty_list(self):
        """Lista vacía: []"""
        program = parse("test([]).")
        
        clause = program.clauses[0]
        list_arg = clause.head.args[0]
        assert isinstance(list_arg, Atom)
        assert list_arg.value == "[]"
    
    def test_simple_list(self):
        """Lista simple: [1, 2, 3]"""
        program = parse("lista([1, 2, 3]).")
        
        clause = program.clauses[0]
        list_arg = clause.head.args[0]
        # Debe ser estructura de punto
        assert isinstance(list_arg, Compound)
        assert list_arg.functor == "."
    
    def test_list_with_tail(self):
        """Lista con cola: [H|T]"""
        program = parse("cons([H|T]).")
        
        clause = program.clauses[0]
        list_arg = clause.head.args[0]
        assert isinstance(list_arg, Compound)
        assert list_arg.functor == "."
        
        # Debe tener variables H y T
        vars_set = list_arg.get_variables()
        var_names = {v.name for v in vars_set}
        assert "H" in var_names
        assert "T" in var_names
    
    def test_list_with_multiple_elements_and_tail(self):
        """Lista: [A, B|Rest]"""
        program = parse("test([A, B|Rest]).")
        
        clause = program.clauses[0]
        list_arg = clause.head.args[0]
        vars_set = list_arg.get_variables()
        var_names = {v.name for v in vars_set}
        assert "A" in var_names
        assert "B" in var_names
        assert "Rest" in var_names


class TestComplexPrograms:
    """Tests de programas completos"""
    
    def test_family_relations(self):
        """Programa de relaciones familiares"""
        code = """
        padre(juan, maria).
        padre(juan, pedro).
        madre(ana, maria).
        
        abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """
        program = parse(code)
        
        assert len(program.clauses) == 4
        facts = [c for c in program.clauses if c.is_fact()]
        rules = [c for c in program.clauses if c.is_rule()]
        assert len(facts) == 3
        assert len(rules) == 1
    
    def test_list_operations(self):
        """Operaciones con listas"""
        code = """
        append([], L, L).
        append([H|T], L, [H|R]) :- append(T, L, R).
        """
        program = parse(code)
        
        assert len(program.clauses) == 2
        assert program.clauses[0].is_fact()
        assert program.clauses[1].is_rule()
    
    def test_program_with_comments(self):
        """Programa con comentarios"""
        code = """
        % Este es un comentario
        padre(juan, maria). % Comentario inline
        % Otro comentario
        madre(ana, maria).
        """
        program = parse(code)
        
        # Comentarios deben ser ignorados
        assert len(program.clauses) == 2


class TestErrorHandling:
    """Tests de manejo de errores"""
    
    def test_missing_dot(self):
        """Error: falta punto al final"""
        with pytest.raises(ParseError):
            parse("padre(juan, maria)")
    
    def test_missing_closing_paren(self):
        """Error: falta paréntesis de cierre"""
        with pytest.raises(ParseError):
            parse("padre(juan, maria.")
    
    def test_invalid_clause_head(self):
        """Error: cabeza de cláusula no es compuesto"""
        with pytest.raises(ParseError):
            parse("X.")
    
    def test_malformed_rule(self):
        """Error: regla mal formada"""
        with pytest.raises(ParseError):
            parse("padre(X) :- .")
    
    def test_invalid_list(self):
        """Error: lista mal formada"""
        with pytest.raises(ParseError):
            parse("test([1, 2,]).")
    
    def test_error_message_includes_location(self):
        """Error debe incluir ubicación"""
        try:
            parse("padre(juan, maria")
        except ParseError as e:
            error_msg = str(e)
            assert "line" in error_msg.lower() or "column" in error_msg.lower()


class TestEdgeCases:
    """Tests de casos edge"""
    
    def test_empty_program(self):
        """Programa vacío"""
        program = parse("")
        assert len(program.clauses) == 0
    
    def test_whitespace_only(self):
        """Solo whitespace"""
        program = parse("   \n\n   ")
        assert len(program.clauses) == 0
    
    def test_only_comments(self):
        """Solo comentarios"""
        program = parse("% Comentario 1\n% Comentario 2")
        assert len(program.clauses) == 0
    
    def test_nested_compounds(self):
        """Compuestos anidados"""
        program = parse("test(f(g(h(a)))).")
        clause = program.clauses[0]
        
        # Debe haber anidamiento
        arg = clause.head.args[0]
        assert isinstance(arg, Compound)
        assert arg.functor == "f"
    
    def test_long_argument_list(self):
        """Lista larga de argumentos"""
        program = parse("test(a, b, c, d, e, f, g).")
        clause = program.clauses[0]
        assert clause.head.arity == 7


class TestGetClausesForPredicate:
    """Tests del método get_clauses_for_predicate"""
    
    def test_get_clauses(self):
        """Obtener cláusulas de un predicado específico"""
        code = """
        padre(juan, maria).
        padre(pedro, ana).
        madre(ana, maria).
        """
        program = parse(code)
        
        padre_clauses = program.get_clauses_for_predicate("padre", 2)
        assert len(padre_clauses) == 2
        
        madre_clauses = program.get_clauses_for_predicate("madre", 2)
        assert len(madre_clauses) == 1
    
    def test_no_matching_clauses(self):
        """No hay cláusulas que coincidan"""
        program = parse("padre(juan, maria).")
        clauses = program.get_clauses_for_predicate("madre", 2)
        assert len(clauses) == 0


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])