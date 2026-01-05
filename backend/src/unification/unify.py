"""
unify.py - Algoritmo de Unificación de Robinson

Implementa el algoritmo de unificación más general (MGU - Most General Unifier)
según el algoritmo clásico de Robinson.

El algoritmo determina si dos términos pueden hacerse idénticos mediante
sustituciones de variables, y si es posible, encuentra la sustitución más
general que los unifica.

Referencia: Robinson, J.A. (1965). "A Machine-Oriented Logic Based on the 
Resolution Principle"
"""

from typing import Optional, Tuple
from parser.terms import (
    Term, Variable, Atom, Number, Compound, 
    List, Substitution
)


class UnificationError(Exception):
    """Excepción lanzada cuando dos términos no pueden unificarse"""
    pass


def occurs_check(var: Variable, term: Term, subst: Substitution) -> bool:
    """
    Verifica si una variable ocurre en un término (occur check).
    
    Esto previene unificaciones infinitas como X = f(X).
    
    Args:
        var: Variable a buscar
        term: Término donde buscar
        subst: Sustitución actual
    
    Returns:
        True si la variable ocurre en el término
    
    Ejemplo:
        X = Variable("X")
        t = Compound("f", (X,))
        occurs_check(X, t, {}) -> True  # X aparece en f(X)
    """
    # Aplicar sustitución al término
    term = term.apply_substitution(subst)
    
    if isinstance(term, Variable):
        return var == term
    elif isinstance(term, (Atom, Number)):
        return False
    elif isinstance(term, Compound):
        return any(occurs_check(var, arg, subst) for arg in term.args)
    
    return False


def deref(term: Term, subst: Substitution) -> Term:
    """
    Desreferencia una variable siguiendo la cadena de sustituciones.
    
    Si X -> Y y Y -> juan, entonces deref(X) = juan
    
    Args:
        term: Término a desreferenciar
        subst: Sustitución actual
    
    Returns:
        Término final después de seguir todas las sustituciones
    """
    while isinstance(term, Variable) and term in subst:
        term = subst[term]
    return term


def unify(term1: Term, term2: Term, subst: Optional[Substitution] = None) -> Substitution:
    """
    Algoritmo de unificación de Robinson.
    
    Encuentra la sustitución más general (MGU) que hace que term1 y term2
    sean idénticos, o lanza UnificationError si no es posible.
    
    Args:
        term1: Primer término
        term2: Segundo término
        subst: Sustitución inicial (vacía si no se proporciona)
    
    Returns:
        Sustitución que unifica los términos
    
    Raises:
        UnificationError: Si los términos no pueden unificarse
    
    Algoritmo:
        1. Desreferenciar ambos términos
        2. Si son idénticos, retornar sustitución actual
        3. Si uno es variable, intentar binding (con occur check)
        4. Si ambos son compuestos con mismo functor/arity, unificar args
        5. Caso contrario: fallo
    
    Ejemplos:
        # Caso 1: Variable con átomo
        unify(Variable("X"), Atom("juan")) 
        -> {X ↦ juan}
        
        # Caso 2: Compuestos
        unify(Compound("f", (Variable("X"), Atom("a"))),
                Compound("f", (Atom("b"), Atom("a"))))
        -> {X ↦ b}
        
        # Caso 3: Fallo
        unify(Atom("juan"), Atom("maria"))
        -> UnificationError
    """
    if subst is None:
        subst = Substitution()
    
    # Paso 1: Desreferenciar términos
    term1 = deref(term1, subst)
    term2 = deref(term2, subst)
    
    # Paso 2: Si son el mismo término, éxito
    if term1 == term2:
        return subst
    
    # Paso 3: Si term1 es variable
    if isinstance(term1, Variable):
        return unify_variable(term1, term2, subst)
    
    # Paso 4: Si term2 es variable
    if isinstance(term2, Variable):
        return unify_variable(term2, term1, subst)
    
    # Paso 5: Ambos son átomos
    if isinstance(term1, Atom) and isinstance(term2, Atom):
        if term1.value == term2.value:
            return subst
        raise UnificationError(f"Cannot unify atoms {term1} and {term2}")
    
    # Paso 6: Ambos son números
    if isinstance(term1, Number) and isinstance(term2, Number):
        if term1.value == term2.value:
            return subst
        raise UnificationError(f"Cannot unify numbers {term1} and {term2}")
    
    # Paso 7: Ambos son compuestos
    if isinstance(term1, Compound) and isinstance(term2, Compound):
        # Deben tener mismo functor y aridad
        if term1.functor != term2.functor or term1.arity != term2.arity:
            raise UnificationError(
                f"Cannot unify {term1} and {term2}: "
                f"different functor/arity"
            )
        
        # Unificar argumentos uno por uno
        for arg1, arg2 in zip(term1.args, term2.args):
            subst = unify(arg1, arg2, subst)
        
        return subst
    
    # Paso 8: Tipos incompatibles
    raise UnificationError(f"Cannot unify {term1} and {term2}: incompatible types")


def unify_variable(var: Variable, term: Term, subst: Substitution) -> Substitution:
    """
    Unifica una variable con un término.
    
    Args:
        var: Variable a unificar
        term: Término con el que unificar
        subst: Sustitución actual
    
    Returns:
        Nueva sustitución con el binding agregado
    
    Raises:
        UnificationError: Si falla el occur check
    """
    # Si la variable ya está en la sustitución, unificar su valor con term
    if var in subst:
        return unify(subst[var], term, subst)
    
    # Si term es variable y ya está en sustitución
    if isinstance(term, Variable) and term in subst:
        return unify(var, subst[term], subst)
    
    # Occur check: prevenir X = f(...X...)
    if occurs_check(var, term, subst):
        raise UnificationError(
            f"Occur check failed: {var} occurs in {term}"
        )
    
    # Crear nueva sustitución con el binding
    new_subst = Substitution(subst)
    new_subst[var] = term
    return new_subst


def unify_goals(goal1: Compound, goal2: Compound, 
                subst: Optional[Substitution] = None) -> Substitution:
    """
    Wrapper especializado para unificar goals (predicados).
    
    Es equivalente a unify() pero con validaciones específicas para goals.
    
    Args:
        goal1: Primer goal (debe ser Compound)
        goal2: Segundo goal (debe ser Compound)
        subst: Sustitución inicial
    
    Returns:
        Sustitución que unifica los goals
    """
    if not isinstance(goal1, Compound) or not isinstance(goal2, Compound):
        raise ValueError("Goals must be Compound terms")
    
    return unify(goal1, goal2, subst)


def rename_variables(term: Term, suffix: str) -> Term:
    """
    Renombra todas las variables en un término agregando un sufijo.
    
    Esto es útil para evitar colisiones de variables entre cláusulas.
    
    Args:
        term: Término a renombrar
        suffix: Sufijo a agregar (e.g., "_1", "_2")
    
    Returns:
        Término con variables renombradas
    
    Ejemplo:
        term = Compound("padre", (Variable("X"), Variable("Y")))
        rename_variables(term, "_1")
        -> padre(X_1, Y_1)
    """
    if isinstance(term, Variable):
        return Variable(f"{term.name}{suffix}")
    elif isinstance(term, (Atom, Number)):
        return term
    elif isinstance(term, Compound):
        new_args = tuple(rename_variables(arg, suffix) for arg in term.args)
        return Compound(term.functor, new_args)
    
    return term


# ============================================================================
# FUNCIONES DE TESTING Y DEBUGGING
# ============================================================================

def unify_verbose(term1: Term, term2: Term, 
                  subst: Optional[Substitution] = None) -> Tuple[bool, Optional[Substitution]]:
    """
    Versión verbose de unify que no lanza excepciones.
    
    Returns:
        (éxito, sustitución o None)
    """
    try:
        result = unify(term1, term2, subst)
        return (True, result)
    except UnificationError as e:
        return (False, None)


def test_unification():
    """Conjunto de tests para verificar el algoritmo de unificación"""
    
    print("=" * 60)
    print("TESTS DE UNIFICACIÓN")
    print("=" * 60)
    
    # Test 1: Variable con átomo
    print("\n1. Variable con átomo:")
    X = Variable("X")
    juan = Atom("juan")
    success, subst = unify_verbose(X, juan)
    print(f"   unify({X}, {juan})")
    print(f"   -> {subst if success else 'FALLO'}")
    
    # Test 2: Compuestos simples
    print("\n2. Compuestos simples:")
    t1 = Compound("padre", (X, Atom("maria")))
    t2 = Compound("padre", (juan, Atom("maria")))
    success, subst = unify_verbose(t1, t2)
    print(f"   unify({t1}, {t2})")
    print(f"   -> {subst if success else 'FALLO'}")
    
    # Test 3: Múltiples variables
    print("\n3. Múltiples variables:")
    Y = Variable("Y")
    Z = Variable("Z")
    t1 = Compound("f", (X, Y, Z))
    t2 = Compound("f", (juan, juan, Atom("carlos")))
    success, subst = unify_verbose(t1, t2)
    print(f"   unify({t1}, {t2})")
    print(f"   -> {subst if success else 'FALLO'}")
    
    # Test 4: Occur check
    print("\n4. Occur check (debe fallar):")
    t1 = X
    t2 = Compound("f", (X,))
    success, subst = unify_verbose(t1, t2)
    print(f"   unify({t1}, {t2})")
    print(f"   -> {subst if success else 'FALLO (occur check)'}")
    
    # Test 5: Átomos diferentes (debe fallar)
    print("\n5. Átomos diferentes (debe fallar):")
    success, subst = unify_verbose(juan, Atom("maria"))
    print(f"   unify({juan}, maria)")
    print(f"   -> {subst if success else 'FALLO'}")
    
    # Test 6: Listas
    print("\n6. Listas:")
    lista1 = List((X, Number(2))).to_compound()
    lista2 = List((Number(1), Number(2))).to_compound()
    success, subst = unify_verbose(lista1, lista2)
    print(f"   unify({lista1}, {lista2})")
    print(f"   -> {subst if success else 'FALLO'}")
    
    # Test 7: Renombramiento de variables
    print("\n7. Renombramiento de variables:")
    term = Compound("padre", (Variable("X"), Variable("Y")))
    renamed = rename_variables(term, "_1")
    print(f"   Original: {term}")
    print(f"   Renombrado: {renamed}")
    
    print("\n" + "=" * 60)


if __name__ == "__main__":
    test_unification()