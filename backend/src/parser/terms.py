"""
terms.py - Representación de términos Prolog en Python

Este módulo define las clases base para representar términos Prolog:
- Variables: X, Y, _result
- Átomos: juan, rojo, []
- Números: 42, 3.14
- Compuestos: padre(juan, maria), [1,2,3]
"""

from dataclasses import dataclass, field
from typing import Any, List, Optional
from abc import ABC, abstractmethod


class Term(ABC):
    """Clase base abstracta para todos los términos Prolog"""
    
    @abstractmethod
    def __str__(self) -> str:
        """Representación en string del término"""
        pass
    
    @abstractmethod
    def is_ground(self) -> bool:
        """Verifica si el término no contiene variables"""
        pass
    
    @abstractmethod
    def get_variables(self) -> set:
        """Retorna el conjunto de variables en el término"""
        pass
    
    @abstractmethod
    def apply_substitution(self, subst: 'Substitution') -> 'Term':
        """Aplica una sustitución al término"""
        pass


@dataclass(frozen=True)
class Variable(Term):
    """
    Representa una variable Prolog: X, Y, _result, etc.
    
    Attributes:
        name: Nombre de la variable (debe empezar con mayúscula o _)
    
    Ejemplo:
        X = Variable("X")
        _result = Variable("_result")
    """
    name: str
    
    def __post_init__(self):
        if not (self.name[0].isupper() or self.name[0] == '_'):
            raise ValueError(f"Variable debe empezar con mayúscula o _: {self.name}")
    
    def __str__(self) -> str:
        return self.name
    
    def is_ground(self) -> bool:
        return False
    
    def get_variables(self) -> set:
        return {self}
    
    def apply_substitution(self, subst: 'Substitution') -> Term:
        """
        Aplica sustitución a la variable.
        Si X -> juan en subst, retorna Atom("juan")
        """
        return subst.get(self, self)
    
    def __hash__(self):
        return hash(self.name)


@dataclass(frozen=True)
class Atom(Term):
    """
    Representa un átomo Prolog: juan, rojo, 'hola mundo'
    
    Attributes:
        value: Valor del átomo (string)
    
    Ejemplo:
        juan = Atom("juan")
        rojo = Atom("rojo")
    """
    value: str
    
    def __str__(self) -> str:
        # Si contiene espacios o caracteres especiales, usar comillas
        if ' ' in self.value or any(c in self.value for c in "()[].,"):
            return f"'{self.value}'"
        return self.value
    
    def is_ground(self) -> bool:
        return True
    
    def get_variables(self) -> set:
        return set()
    
    def apply_substitution(self, subst: 'Substitution') -> Term:
        return self  # Átomos no cambian con sustituciones
    
    def __hash__(self):
        return hash(self.value)


@dataclass(frozen=True)
class Number(Term):
    """
    Representa un número Prolog: 42, 3.14, -5
    
    Attributes:
        value: Valor numérico (int o float)
    """
    value: float | int
    
    def __str__(self) -> str:
        return str(self.value)
    
    def is_ground(self) -> bool:
        return True
    
    def get_variables(self) -> set:
        return set()
    
    def apply_substitution(self, subst: 'Substitution') -> Term:
        return self
    
    def __hash__(self):
        return hash(self.value)


@dataclass(frozen=True)
class Compound(Term):
    """
    Representa un término compuesto: padre(juan, maria), suma(2, 3, X)
    
    Attributes:
        functor: Nombre del functor (átomo)
        args: Lista de argumentos (términos)
    
    Ejemplo:
        padre(juan, maria) = Compound("padre", [Atom("juan"), Atom("maria")])
        suma(X, Y, Z) = Compound("suma", [Variable("X"), Variable("Y"), Variable("Z")])
    """
    functor: str
    args: tuple[Term, ...] = field(default_factory=tuple)
    
    def __post_init__(self):
        # Convertir lista a tupla para inmutabilidad
        if isinstance(self.args, list):
            object.__setattr__(self, 'args', tuple(self.args))
    
    @property
    def arity(self) -> int:
        """Retorna la aridad del compuesto (número de argumentos)"""
        return len(self.args)
    
    def __str__(self) -> str:
        if self.arity == 0:
            return self.functor
        args_str = ", ".join(str(arg) for arg in self.args)
        return f"{self.functor}({args_str})"
    
    def is_ground(self) -> bool:
        """Es ground si todos sus argumentos son ground"""
        return all(arg.is_ground() for arg in self.args)
    
    def get_variables(self) -> set:
        """Retorna todas las variables en los argumentos"""
        variables = set()
        for arg in self.args:
            variables.update(arg.get_variables())
        return variables
    
    def apply_substitution(self, subst: 'Substitution') -> Term:
        """Aplica sustitución recursivamente a todos los argumentos"""
        new_args = tuple(arg.apply_substitution(subst) for arg in self.args)
        return Compound(self.functor, new_args)
    
    def __hash__(self):
        return hash((self.functor, self.args))


@dataclass(frozen=True)
class List(Term):
    """
    Representa una lista Prolog: [], [1,2,3], [H|T]
    
    Internamente se representa como:
    - [] = Atom("[]")
    - [H|T] = Compound(".", [H, T])
    
    Esta clase es un wrapper conveniente para construcción.
    
    Attributes:
        elements: Lista de elementos
        tail: Cola de la lista (None para lista cerrada, Variable para abierta)
    
    Ejemplo:
        [1,2,3] = List([Number(1), Number(2), Number(3)])
        [H|T] = List([Variable("H")], tail=Variable("T"))
    """
    elements: tuple[Term, ...] = field(default_factory=tuple)
    tail: Optional[Term] = None
    
    def __post_init__(self):
        if isinstance(self.elements, list):
            object.__setattr__(self, 'elements', tuple(self.elements))
    
    def to_compound(self) -> Term:
        """
        Convierte la lista a su representación interna en Prolog.
        
        [] -> Atom("[]")
        [1,2,3] -> Compound(".", [Number(1), Compound(".", [Number(2), 
                            Compound(".", [Number(3), Atom("[]")])])])
        [H|T] -> Compound(".", [Variable("H"), Variable("T")])
        """
        if len(self.elements) == 0 and self.tail is None:
            return Atom("[]")
        
        # Construir desde el final
        result = self.tail if self.tail else Atom("[]")
        for elem in reversed(self.elements):
            result = Compound(".", (elem, result))
        
        return result
    
    def __str__(self) -> str:
        if len(self.elements) == 0 and self.tail is None:
            return "[]"
        
        elements_str = ", ".join(str(e) for e in self.elements)
        if self.tail:
            return f"[{elements_str}|{self.tail}]"
        return f"[{elements_str}]"
    
    def is_ground(self) -> bool:
        elements_ground = all(e.is_ground() for e in self.elements)
        tail_ground = self.tail.is_ground() if self.tail else True
        return elements_ground and tail_ground
    
    def get_variables(self) -> set:
        variables = set()
        for elem in self.elements:
            variables.update(elem.get_variables())
        if self.tail:
            variables.update(self.tail.get_variables())
        return variables
    
    def apply_substitution(self, subst: 'Substitution') -> Term:
        new_elements = tuple(e.apply_substitution(subst) for e in self.elements)
        new_tail = self.tail.apply_substitution(subst) if self.tail else None
        return List(new_elements, new_tail).to_compound()
    
    def __hash__(self):
        return hash((self.elements, self.tail))


# Alias para importación conveniente
from typing import Dict

class Substitution(dict):
    """
    Representa una sustitución θ: Variable -> Term
    
    Ejemplo:
        θ = Substitution({Variable("X"): Atom("juan"), 
                        Variable("Y"): Number(42)})
    """
    
    def apply(self, term: Term) -> Term:
        """Aplica la sustitución a un término"""
        return term.apply_substitution(self)
    
    def compose(self, other: 'Substitution') -> 'Substitution':
        """
        Composición de sustituciones θ₁ ∘ θ₂
        
        (θ₁ ∘ θ₂)(X) = θ₁(θ₂(X))
        
        Algoritmo:
        1. Para cada variable en θ₂, aplicar θ₁ a su valor
        2. Para cada variable en θ₁ que NO está en θ₂, aplicar θ₂ a su valor
        """
        result = Substitution()
        
        # Paso 1: Aplicar θ₁ a los valores de θ₂
        for var, term in other.items():
            result[var] = term.apply_substitution(self)
        
        # Paso 2: Agregar bindings de θ₁ que no están en θ₂
        # IMPORTANTE: También aplicar θ₂ a estos términos
        for var, term in self.items():
            if var not in result:
                result[var] = term.apply_substitution(other)
        
        return result
    
    def __str__(self) -> str:
        if not self:
            return "{}"
        items = [f"{var} ↦ {term}" for var, term in self.items()]
        return "{" + ", ".join(items) + "}"


# ============================================================================
# EJEMPLOS DE USO
# ============================================================================

if __name__ == "__main__":
    # Variables
    X = Variable("X")
    Y = Variable("Y")
    Z = Variable("Z")
    
    # Átomos
    juan = Atom("juan")
    maria = Atom("maria")
    
    # Números
    n1 = Number(1)
    n2 = Number(2)
    
    # Compuestos
    padre_juan_maria = Compound("padre", (juan, maria))
    print(f"Compuesto: {padre_juan_maria}")  # padre(juan, maria)
    
    # Listas
    lista1 = List((n1, n2, Number(3)))
    print(f"Lista: {lista1}")  # [1, 2, 3]
    
    lista2 = List((X,), tail=Y)
    print(f"Lista abierta: {lista2}")  # [X|Y]
    
    # Sustituciones
    subst = Substitution({X: juan, Y: maria})
    print(f"\nSustitución: {subst}")
    
    # Aplicar sustitución
    term = Compound("padre", (X, Y))
    print(f"Término original: {term}")
    result = term.apply_substitution(subst)
    print(f"Después de aplicar θ: {result}")  # padre(juan, maria)
    
    # Variables en un término
    term2 = Compound("suma", (X, Number(2), Z))
    print(f"\nVariables en {term2}: {term2.get_variables()}")