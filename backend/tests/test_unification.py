"""
test_unification.py - Tests del algoritmo de unificación

Tests esenciales:
- Unificación básica
- Occur check
- Renombramiento de variables
- Casos edge
"""

import pytest
from src.parser.terms import Variable, Atom, Number, Compound, List, Substitution
from src.unification.unify import (
    unify, unify_variable, rename_variables,
    occurs_check, deref, UnificationError
)


class TestBasicUnification:
    """Tests de unificación básica"""
    
    def test_unify_identical_atoms(self):
        """Átomos idénticos unifican"""
        a1 = Atom("juan")
        a2 = Atom("juan")
        subst = unify(a1, a2)
        assert subst == {}
    
    def test_unify_different_atoms_fails(self):
        """Átomos diferentes no unifican"""
        a1 = Atom("juan")
        a2 = Atom("maria")
        with pytest.raises(UnificationError):
            unify(a1, a2)
    
    def test_unify_identical_numbers(self):
        """Números idénticos unifican"""
        n1 = Number(42)
        n2 = Number(42)
        subst = unify(n1, n2)
        assert subst == {}
    
    def test_unify_different_numbers_fails(self):
        """Números diferentes no unifican"""
        n1 = Number(42)
        n2 = Number(43)
        with pytest.raises(UnificationError):
            unify(n1, n2)


class TestVariableUnification:
    """Tests de unificación con variables"""
    
    def test_unify_variable_with_atom(self):
        """Variable unifica con átomo"""
        X = Variable("X")
        juan = Atom("juan")
        subst = unify(X, juan)
        
        assert X in subst
        assert subst[X] == juan
    
    def test_unify_variable_with_number(self):
        """Variable unifica con número"""
        X = Variable("X")
        n = Number(42)
        subst = unify(X, n)
        
        assert X in subst
        assert subst[X] == n
    
    def test_unify_two_variables(self):
        """Dos variables unifican entre sí"""
        X = Variable("X")
        Y = Variable("Y")
        subst = unify(X, Y)
        
        # Una debe estar ligada a la otra
        assert len(subst) >= 1
    
    def test_unify_variable_already_bound(self):
        """Variable ya ligada debe unificar su valor"""
        X = Variable("X")
        Y = Variable("Y")
        initial_subst = Substitution({X: Atom("juan")})
        
        result = unify(X, Y, initial_subst)
        
        # Y debe ligarse a juan (o X)
        assert Y in result or X in result


class TestCompoundUnification:
    """Tests de unificación de compuestos"""
    
    def test_unify_identical_compounds(self):
        """Compuestos idénticos unifican"""
        c1 = Compound("f", (Atom("a"), Atom("b")))
        c2 = Compound("f", (Atom("a"), Atom("b")))
        subst = unify(c1, c2)
        assert subst == {}
    
    def test_unify_compounds_different_functor_fails(self):
        """Compuestos con diferente functor no unifican"""
        c1 = Compound("f", (Atom("a"),))
        c2 = Compound("g", (Atom("a"),))
        with pytest.raises(UnificationError):
            unify(c1, c2)
    
    def test_unify_compounds_different_arity_fails(self):
        """Compuestos con diferente aridad no unifican"""
        c1 = Compound("f", (Atom("a"),))
        c2 = Compound("f", (Atom("a"), Atom("b")))
        with pytest.raises(UnificationError):
            unify(c1, c2)
    
    def test_unify_compounds_with_variables(self):
        """Unificar compuestos con variables"""
        X = Variable("X")
        c1 = Compound("padre", (X, Atom("maria")))
        c2 = Compound("padre", (Atom("juan"), Atom("maria")))
        
        subst = unify(c1, c2)
        
        assert X in subst
        assert subst[X] == Atom("juan")
    
    def test_unify_nested_compounds(self):
        """Unificar compuestos anidados"""
        X = Variable("X")
        c1 = Compound("f", (Compound("g", (X,)),))
        c2 = Compound("f", (Compound("g", (Atom("a"),)),))
        
        subst = unify(c1, c2)
        assert X in subst
        assert subst[X] == Atom("a")


class TestOccurCheck:
    """Tests del occur check"""
    
    def test_occur_check_prevents_infinite_term(self):
        """Occur check previene términos infinitos"""
        X = Variable("X")
        f_x = Compound("f", (X,))
        
        # X = f(X) debe fallar
        with pytest.raises(UnificationError) as excinfo:
            unify(X, f_x)
        assert "occur" in str(excinfo.value).lower()
    
    def test_occur_check_in_nested_structure(self):
        """Occur check en estructura anidada"""
        X = Variable("X")
        term = Compound("f", (Compound("g", (X,)),))
        
        with pytest.raises(UnificationError):
            unify(X, term)
    
    def test_occur_check_function(self):
        """Test directo de la función occurs_check"""
        X = Variable("X")
        term = Compound("f", (X,))
        
        assert occurs_check(X, term, Substitution())


class TestListUnification:
    """Tests de unificación de listas"""
    
    def test_unify_empty_lists(self):
        """Listas vacías unifican"""
        l1 = Atom("[]")
        l2 = Atom("[]")
        subst = unify(l1, l2)
        assert subst == {}
    
    def test_unify_simple_lists(self):
        """Listas simples con variables"""
        X = Variable("X")
        list1 = List((X, Number(2)), None).to_compound()
        list2 = List((Number(1), Number(2)), None).to_compound()
        
        subst = unify(list1, list2)
        assert X in subst
        assert subst[X] == Number(1)
    
    def test_unify_lists_with_tail(self):
        """Listas con cola [H|T]"""
        H = Variable("H")
        T = Variable("T")
        list1 = List((H,), T).to_compound()
        list2 = List((Number(1), Number(2), Number(3)), None).to_compound()
        
        subst = unify(list1, list2)
        
        # H debe ser 1
        assert H in subst
        assert subst[H] == Number(1)
        
        # T debe ser [2, 3]
        assert T in subst


class TestDerefAndSubstitution:
    """Tests de desreferenciación y aplicación de sustituciones"""
    
    def test_deref_unbound_variable(self):
        """Desreferenciar variable no ligada"""
        X = Variable("X")
        subst = Substitution()
        result = deref(X, subst)
        assert result == X
    
    def test_deref_bound_variable(self):
        """Desreferenciar variable ligada"""
        X = Variable("X")
        subst = Substitution({X: Atom("juan")})
        result = deref(X, subst)
        assert result == Atom("juan")
    
    def test_deref_chain(self):
        """Desreferenciar cadena de variables"""
        X = Variable("X")
        Y = Variable("Y")
        Z = Variable("Z")
        subst = Substitution({X: Y, Y: Z, Z: Atom("juan")})
        
        result = deref(X, subst)
        assert result == Atom("juan")
    
    def test_deref_non_variable(self):
        """Desreferenciar no-variable retorna el término"""
        atom = Atom("juan")
        result = deref(atom, Substitution())
        assert result == atom


class TestRenameVariables:
    """Tests de renombramiento de variables"""
    
    def test_rename_single_variable(self):
        """Renombrar variable simple"""
        X = Variable("X")
        renamed = rename_variables(X, "_1")
        
        assert isinstance(renamed, Variable)
        assert renamed.name == "X_1"
    
    def test_rename_compound_with_variables(self):
        """Renombrar variables en compuesto"""
        X = Variable("X")
        Y = Variable("Y")
        term = Compound("padre", (X, Y))
        
        renamed = rename_variables(term, "_1")
        
        vars_set = renamed.get_variables()
        var_names = {v.name for v in vars_set}
        assert "X_1" in var_names
        assert "Y_1" in var_names
    
    def test_rename_preserves_atoms(self):
        """Renombrar no afecta átomos"""
        term = Compound("padre", (Atom("juan"), Variable("X")))
        renamed = rename_variables(term, "_1")
        
        # El átomo debe seguir igual
        assert renamed.args[0] == Atom("juan")
        
        # La variable debe estar renombrada
        assert renamed.args[1].name == "X_1"


class TestComplexUnifications:
    """Tests de unificaciones complejas"""
    
    def test_unify_family_predicate(self):
        """Unificar predicado familiar"""
        X = Variable("X")
        query = Compound("padre", (X, Atom("maria")))
        fact = Compound("padre", (Atom("juan"), Atom("maria")))
        
        subst = unify(query, fact)
        
        assert X in subst
        assert subst[X] == Atom("juan")
    
    def test_unify_with_multiple_variables(self):
        """Unificar con múltiples variables"""
        X = Variable("X")
        Y = Variable("Y")
        Z = Variable("Z")
        
        term1 = Compound("f", (X, Y, Z))
        term2 = Compound("f", (Atom("a"), Atom("b"), Atom("c")))
        
        subst = unify(term1, term2)
        
        assert subst[X] == Atom("a")
        assert subst[Y] == Atom("b")
        assert subst[Z] == Atom("c")
    
    def test_unify_append_example(self):
        """Ejemplo real de append"""
        H = Variable("H")
        T = Variable("T")
        R = Variable("R")
        L = Variable("L")
        
        # append([H|T], L, [H|R])
        clause_head = Compound(
            "append",
            (
                List((H,), T).to_compound(),
                L,
                List((H,), R).to_compound()
            )
        )
        
        # append([1,2], [3], X)
        query = Compound(
            "append",
            (
                List((Number(1), Number(2)), None).to_compound(),
                List((Number(3),), None).to_compound(),
                Variable("X")
            )
        )
        
        # Renombrar variables de la cláusula
        renamed_head = rename_variables(clause_head, "_1")
        
        # Unificar
        subst = unify(query, renamed_head)
        
        # Debe haber ligado variables
        assert len(subst) > 0


class TestEdgeCases:
    """Tests de casos edge"""
    
    def test_unify_with_empty_substitution(self):
        """Unificar con sustitución vacía"""
        X = Variable("X")
        subst = unify(X, Atom("a"), Substitution())
        assert X in subst
    
    def test_unify_same_variable_twice(self):
        """Unificar la misma variable consigo misma"""
        X = Variable("X")
        subst = unify(X, X)
        # Debe ser exitoso (sustitución vacía o con X)
        assert isinstance(subst, Substitution)
    
    def test_incompatible_types(self):
        """Tipos incompatibles no unifican"""
        with pytest.raises(UnificationError):
            unify(Atom("a"), Number(1))


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])