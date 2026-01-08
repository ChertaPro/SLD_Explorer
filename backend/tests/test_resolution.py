"""
test_resolution.py - Tests del motor de resolución SLD

Tests esenciales:
- Resolución de consultas simples
- Resolución con reglas
- Detección de soluciones
- Estrategias de selección
"""

import pytest
from src.parser import parse, parse_query
from src.sld import SLDResolver, NodeStatus


class TestSimpleResolution:
    """Tests de resolución simple"""
    
    def test_single_fact_match(self):
        """Consulta que coincide con un hecho"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, maria).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 1
    
    def test_single_fact_with_variable(self):
        """Consulta con variable"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 1
        
        # X debe estar ligada a maria
        _, subst = solutions[0]
        # Verificar que hay una sustitución
        assert len(subst) > 0
    
    def test_no_matching_fact(self):
        """Consulta sin coincidencias"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(pedro, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 0


class TestMultipleSolutions:
    """Tests con múltiples soluciones"""
    
    def test_multiple_facts_match(self):
        """Múltiples hechos coinciden"""
        program = parse("""
            padre(juan, maria).
            padre(juan, pedro).
            padre(juan, ana).
        """)
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 3
    
    def test_multiple_variables(self):
        """Consulta con múltiples variables libres"""
        program = parse("""
            padre(juan, maria).
            padre(pedro, ana).
        """)
        query = parse_query("?- padre(X, Y).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 2


class TestRuleResolution:
    """Tests de resolución con reglas"""
    
    def test_simple_rule(self):
        """Regla simple"""
        program = parse("""
            padre(juan, maria).
            padre(pedro, juan).
            
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """)
        query = parse_query("?- abuelo(pedro, maria).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 1
    
    def test_rule_with_variable(self):
        """Regla con variable en consulta"""
        program = parse("""
            padre(juan, maria).
            padre(juan, pedro).
            padre(pedro, ana).
            
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """)
        query = parse_query("?- abuelo(juan, Z).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        # Juan es abuelo de ana
        assert len(solutions) == 1
    
    def test_rule_with_multiple_solutions(self):
        """Regla que genera múltiples soluciones"""
        program = parse("""
            padre(juan, pedro).
            padre(juan, maria).
            padre(pedro, ana).
            padre(pedro, luis).
            padre(maria, carlos).
            
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """)
        query = parse_query("?- abuelo(juan, Z).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        # Juan es abuelo de ana, luis, y carlos
        assert len(solutions) == 3


class TestTreeStructure:
    """Tests de la estructura del árbol"""
    
    def test_root_node_created(self):
        """Nodo raíz se crea correctamente"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, maria).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        assert tree.root is not None
        assert tree.root.depth == 0
    
    def test_success_nodes_marked(self):
        """Nodos exitosos se marcan correctamente"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, maria).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        # Debe haber al menos un nodo SUCCESS
        success_nodes = [n for n in tree.all_nodes if n.status == NodeStatus.SUCCESS]
        assert len(success_nodes) > 0
    
    def test_failure_nodes_marked(self):
        """Nodos fallidos se marcan correctamente"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(pedro, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        # Debe haber nodos FAILURE
        failure_nodes = [n for n in tree.all_nodes if n.status == NodeStatus.FAILURE]
        assert len(failure_nodes) > 0
    
    def test_tree_depth_limited(self):
        """Profundidad del árbol se limita"""
        program = parse("""
            loop(X) :- loop(X).
        """)
        query = parse_query("?- loop(a).")
        
        resolver = SLDResolver(program, max_depth=5)
        tree = resolver.resolve(query.goals)
        
        # No debe exceder max_depth
        max_depth_in_tree = max(n.depth for n in tree.all_nodes)
        assert max_depth_in_tree <= 5


class TestStrategies:
    """Tests de estrategias de selección"""
    
    def test_leftmost_strategy(self):
        """Estrategia leftmost (por defecto)"""
        program = parse("""
            a(1).
            b(2).
        """)
        query = parse_query("?- a(X), b(Y).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals, strategy="leftmost")
        
        solutions = tree.find_solutions()
        assert len(solutions) > 0
    
    def test_rightmost_strategy(self):
        """Estrategia rightmost"""
        program = parse("""
            a(1).
            b(2).
        """)
        query = parse_query("?- a(X), b(Y).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals, strategy="rightmost")
        
        solutions = tree.find_solutions()
        assert len(solutions) > 0


class TestListResolution:
    """Tests con listas"""
    
    def test_append_base_case(self):
        """Caso base de append"""
        program = parse("append([], L, L).")
        query = parse_query("?- append([], [1,2], X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) == 1
    
    def test_member_simple(self):
        """Predicado member simple"""
        program = parse("""
            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """)
        query = parse_query("?- member(2, [1,2,3]).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) >= 1


class TestComplexPrograms:
    """Tests con programas complejos"""
    
    def test_family_tree(self):
        """Árbol familiar completo"""
        program = parse("""
            padre(abuelo, padre).
            padre(padre, hijo).
            madre(abuela, padre).
            madre(madre, hijo).
            
            progenitor(X, Y) :- padre(X, Y).
            progenitor(X, Y) :- madre(X, Y).
            
            ancestro(X, Y) :- progenitor(X, Y).
            ancestro(X, Y) :- progenitor(X, Z), ancestro(Z, Y).
        """)
        query = parse_query("?- ancestro(abuelo, hijo).")
        
        resolver = SLDResolver(program, max_depth=10)
        tree = resolver.resolve(query.goals)
        
        solutions = tree.find_solutions()
        assert len(solutions) >= 1


class TestTreeSerialization:
    """Tests de serialización del árbol"""
    
    def test_to_dict(self):
        """Serializar árbol a diccionario"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        tree_dict = tree.to_dict()
        
        assert "root" in tree_dict
        assert "nodes" in tree_dict
        assert "solutions" in tree_dict
        assert isinstance(tree_dict["nodes"], list)
    
    def test_node_to_dict(self):
        """Serializar nodo a diccionario"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        node_dict = tree.root.to_dict()
        
        assert "id" in node_dict
        assert "goals" in node_dict
        assert "status" in node_dict
        assert "depth" in node_dict


class TestStepByStep:
    """Tests de resolución paso a paso"""
    
    def test_step_by_step_generator(self):
        """Generador paso a paso produce resultados"""
        program = parse("padre(juan, maria).")
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        
        steps = list(resolver.resolve_step_by_step(query.goals))
        
        # Debe haber al menos un paso
        assert len(steps) > 0
        
        # Cada paso debe tener árbol y nodo actual
        for tree, node in steps:
            assert tree is not None
            assert node is not None


class TestEdgeCases:
    """Tests de casos edge"""
    
    def test_empty_program(self):
        """Programa vacío"""
        program = parse("")
        query = parse_query("?- padre(juan, X).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve(query.goals)
        
        # No debe haber soluciones
        solutions = tree.find_solutions()
        assert len(solutions) == 0
    
    def test_query_with_no_goals(self):
        """Consulta sin goals (éxito inmediato)"""
        program = parse("padre(juan, maria).")
        
        resolver = SLDResolver(program)
        tree = resolver.resolve([])  # Lista vacía de goals
        
        # Debe ser éxito inmediato
        assert tree.root.is_success()
    
    def test_very_deep_recursion(self):
        """Recursión muy profunda se limita"""
        program = parse("""
            nat(0).
            nat(s(X)) :- nat(X).
        """)
        query = parse_query("?- nat(X).")
        
        resolver = SLDResolver(program, max_depth=5)
        tree = resolver.resolve(query.goals)
        
        # Debe detenerse en max_depth
        assert all(n.depth <= 5 for n in tree.all_nodes)


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])