"""
test_terms.py - Tests para representación de términos

Tests esenciales:
- Creación de términos
- Métodos de términos (is_ground, get_variables)
- Aplicación de sustituciones
- Composición de sustituciones
"""

import pytest
from src.parser.terms import (
    Variable, Atom, Number, Compound, List, Substitution
)


class TestVariable:
    """Tests de variables"""
    
    def test_create_variable(self):
        """Crear variable simple"""
        x = Variable("X")
        assert x.name == "X"
        assert str(x) == "X"
    
    def test_variable_with_underscore(self):
        """Variable que empieza con underscore"""
        var = Variable("_result")
        assert var.name == "_result"
    
    def test_invalid_variable_name(self):
        """Nombre de variable inválido debe fallar"""
        with pytest.raises(ValueError):
            Variable("lowercase")
    
    def test_variable_not_ground(self):
        """Variables no son ground"""
        x = Variable("X")
        assert not x.is_ground()
    
    def test_variable_get_variables(self):
        """get_variables debe retornar la variable misma"""
        x = Variable("X")
        vars_set = x.get_variables()
        assert x in vars_set
        assert len(vars_set) == 1
    
    def test_variable_equality(self):
        """Dos variables con mismo nombre son iguales"""
        x1 = Variable("X")
        x2 = Variable("X")
        assert x1 == x2
        assert hash(x1) == hash(x2)


class TestAtom:
    """Tests de átomos"""
    
    def test_create_atom(self):
        """Crear átomo simple"""
        atom = Atom("juan")
        assert atom.value == "juan"
        assert str(atom) == "juan"
    
    def test_atom_is_ground(self):
        """Átomos son ground"""
        atom = Atom("maria")
        assert atom.is_ground()
    
    def test_atom_no_variables(self):
        """Átomos no tienen variables"""
        atom = Atom("pedro")
        assert len(atom.get_variables()) == 0
    
    def test_atom_with_special_chars(self):
        """Átomo con caracteres especiales usa comillas"""
        atom = Atom("hola mundo")
        assert "'" in str(atom)


class TestNumber:
    """Tests de números"""
    
    def test_create_integer(self):
        """Crear número entero"""
        num = Number(42)
        assert num.value == 42
        assert str(num) == "42"
    
    def test_create_float(self):
        """Crear número flotante"""
        num = Number(3.14)
        assert num.value == 3.14
        assert str(num) == "3.14"
    
    def test_number_is_ground(self):
        """Números son ground"""
        num = Number(100)
        assert num.is_ground()
    
    def test_number_no_variables(self):
        """Números no tienen variables"""
        num = Number(-5)
        assert len(num.get_variables()) == 0


class TestCompound:
    """Tests de términos compuestos"""
    
    def test_create_compound(self):
        """Crear término compuesto simple"""
        padre = Compound("padre", (Atom("juan"), Atom("maria")))
        assert padre.functor == "padre"
        assert padre.arity == 2
        assert str(padre) == "padre(juan, maria)"
    
    def test_compound_with_variables(self):
        """Compuesto con variables"""
        f = Compound("f", (Variable("X"), Variable("Y")))
        assert f.arity == 2
        assert not f.is_ground()
    
    def test_compound_get_variables(self):
        """Obtener variables de un compuesto"""
        X = Variable("X")
        Y = Variable("Y")
        f = Compound("f", (X, Atom("a"), Y))
        
        vars_set = f.get_variables()
        assert X in vars_set
        assert Y in vars_set
        assert len(vars_set) == 2
    
    def test_nested_compound(self):
        """Compuestos anidados"""
        inner = Compound("g", (Atom("a"),))
        outer = Compound("f", (inner, Atom("b")))
        assert outer.arity == 2
        assert "g(a)" in str(outer)
    
    def test_compound_is_ground(self):
        """Compuesto es ground si todos sus args son ground"""
        ground = Compound("f", (Atom("a"), Number(1)))
        not_ground = Compound("f", (Variable("X"), Atom("a")))
        
        assert ground.is_ground()
        assert not not_ground.is_ground()


class TestList:
    """Tests de listas"""
    
    def test_empty_list(self):
        """Lista vacía"""
        empty = List(tuple(), None)
        result = empty.to_compound()
        assert isinstance(result, Atom)
        assert result.value == "[]"
    
    def test_simple_list(self):
        """Lista simple [1, 2, 3]"""
        lst = List((Number(1), Number(2), Number(3)), None)
        result = lst.to_compound()
        
        # Debe ser una estructura .(1, .(2, .(3, [])))
        assert isinstance(result, Compound)
        assert result.functor == "."
    
    def test_list_with_tail(self):
        """Lista con cola [H|T]"""
        H = Variable("H")
        T = Variable("T")
        lst = List((H,), T)
        result = lst.to_compound()
        
        assert isinstance(result, Compound)
        assert result.functor == "."
        vars_set = result.get_variables()
        assert H in vars_set
        assert T in vars_set
    
    def test_list_is_ground(self):
        """Lista es ground si todos sus elementos lo son"""
        ground_list = List((Number(1), Number(2)), None)
        not_ground_list = List((Variable("X"), Number(2)), None)
        
        assert ground_list.is_ground()
        assert not not_ground_list.is_ground()


class TestSubstitution:
    """Tests de sustituciones"""
    
    def test_create_substitution(self):
        """Crear sustitución simple"""
        X = Variable("X")
        subst = Substitution({X: Atom("juan")})
        assert X in subst
        assert subst[X] == Atom("juan")
    
    def test_apply_substitution_to_variable(self):
        """Aplicar sustitución a variable"""
        X = Variable("X")
        subst = Substitution({X: Atom("maria")})
        
        result = X.apply_substitution(subst)
        assert result == Atom("maria")
    
    def test_apply_substitution_to_compound(self):
        """Aplicar sustitución a compuesto"""
        X = Variable("X")
        Y = Variable("Y")
        term = Compound("f", (X, Y))
        subst = Substitution({X: Atom("a"), Y: Atom("b")})
        
        result = term.apply_substitution(subst)
        assert result == Compound("f", (Atom("a"), Atom("b")))
    
    def test_substitution_composition(self):
        """Composición de sustituciones"""
        X = Variable("X")
        Y = Variable("Y")
        
        subst1 = Substitution({X: Y})
        subst2 = Substitution({Y: Atom("juan")})
        
        composed = subst1.compose(subst2)
        
        # X -> Y en subst1, Y -> juan en subst2
        # Entonces X -> juan en composición
        result = X.apply_substitution(composed)
        assert result == Atom("juan")
    
    def test_substitution_string_representation(self):
        """Representación en string de sustitución"""
        X = Variable("X")
        subst = Substitution({X: Atom("pedro")})
        str_repr = str(subst)
        
        assert "X" in str_repr
        assert "pedro" in str_repr


class TestComplexTerms:
    """Tests de términos complejos"""
    
    def test_family_predicate(self):
        """Predicado familiar: padre(juan, maria)"""
        padre = Compound(
            "padre",
            (Atom("juan"), Atom("maria"))
        )
        
        assert padre.is_ground()
        assert len(padre.get_variables()) == 0
        assert str(padre) == "padre(juan, maria)"
    
    def test_query_with_variables(self):
        """Consulta: padre(X, maria)"""
        X = Variable("X")
        query = Compound("padre", (X, Atom("maria")))
        
        assert not query.is_ground()
        assert X in query.get_variables()
    
    def test_list_append_structure(self):
        """Estructura de append([1,2], [3,4], X)"""
        X = Variable("X")
        list1 = List((Number(1), Number(2)), None).to_compound()
        list2 = List((Number(3), Number(4)), None).to_compound()
        
        append_term = Compound("append", (list1, list2, X))
        
        assert append_term.arity == 3
        assert X in append_term.get_variables()


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])