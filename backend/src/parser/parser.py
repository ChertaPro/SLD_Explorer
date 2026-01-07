"""
parser.py - Parser de Prolog Recursivo Descendente

Responsabilidades:
- Análisis sintáctico de tokens
- Construcción del AST
- Validación de gramática

Gramática (simplificada):
    program    ::= clause*
    clause     ::= fact | rule
    fact       ::= term '.'
    rule       ::= term ':-' body '.'
    body       ::= goal (',' goal)*
    goal       ::= term
    term       ::= atom | variable | number | compound | list
    compound   ::= atom '(' args ')'
    args       ::= term (',' term)*
    list       ::= '[' ']' | '[' list_items ']'
    list_items ::= term (',' term)* ('|' term)?
"""

from typing import List, Optional
from dataclasses import dataclass

from .lexer import Token, Lexer
from .terms import (
    Term, Variable, Atom, Number, Compound, 
    List as ListTerm
)


# ============================================================================
# EXCEPCIONES
# ============================================================================

class ParseError(Exception):
    """Excepción para errores de parsing"""
    
    def __init__(self, message: str, token: Optional[Token] = None):
        if token:
            super().__init__(
                f"{message} at line {token.line}, column {token.column}"
            )
        else:
            super().__init__(message)
        self.token = token


# ============================================================================
# AST NODES
# ============================================================================

@dataclass
class Clause:
    """
    Representa una cláusula Prolog (hecho o regla).
    
    Examples:
        padre(juan, maria).              -> Clause(head=..., body=None)
        abuelo(X, Z) :- padre(X, Y), ... -> Clause(head=..., body=[...])
    """
    head: Compound
    body: Optional[List[Compound]] = None
    
    def is_fact(self) -> bool:
        """Verifica si es un hecho (sin cuerpo)"""
        return self.body is None
    
    def is_rule(self) -> bool:
        """Verifica si es una regla (con cuerpo)"""
        return self.body is not None
    
    def __str__(self) -> str:
        if self.is_fact():
            return f"{self.head}."
        body_str = ", ".join(str(g) for g in self.body)
        return f"{self.head} :- {body_str}."


@dataclass
class Query:
    """
    Representa una consulta Prolog.
    
    Example:
        ?- padre(X, maria).  -> Query(goals=[padre(X, maria)])
    """
    goals: List[Compound]
    
    def __str__(self) -> str:
        goals_str = ", ".join(str(g) for g in self.goals)
        return f"?- {goals_str}."


@dataclass
class Program:
    """
    Representa un programa Prolog completo.
    
    Example:
        padre(juan, maria).
        abuelo(X, Z) :- ...
        -> Program(clauses=[...])
    """
    clauses: List[Clause]
    
    def __str__(self) -> str:
        return "\n".join(str(c) for c in self.clauses)
    
    def get_clauses_for_predicate(self, functor: str, arity: int) -> List[Clause]:
        """
        Retorna todas las cláusulas para un predicado específico.
        
        Args:
            functor: Nombre del predicado
            arity: Aridad del predicado
        
        Returns:
            Lista de cláusulas que coinciden
        """
        return [
            clause for clause in self.clauses
            if clause.head.functor == functor and clause.head.arity == arity
        ]


# ============================================================================
# PARSER
# ============================================================================

class Parser:
    """
    Parser recursivo descendente para Prolog.
    
    Construye el AST a partir de una lista de tokens.
    """
    
    def __init__(self, tokens: List[Token]):
        """
        Inicializa el parser.
        
        Args:
            tokens: Lista de tokens del lexer
        """
        self.tokens = tokens
        self.pos = 0
    
    # ========================================================================
    # NAVEGACIÓN DE TOKENS
    # ========================================================================
    
    def current_token(self) -> Optional[Token]:
        """Retorna el token actual sin consumirlo"""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None
    
    def peek_ahead(self, steps: int = 1) -> Optional[Token]:
        """
        Mira adelante N tokens sin consumir.
        
        Args:
            steps: Cuántos tokens adelante mirar
        
        Returns:
            Token en esa posición o None
        """
        pos = self.pos + steps
        if pos < len(self.tokens):
            return self.tokens[pos]
        return None
    
    def consume(self, expected_type: str) -> Token:
        """
        Consume un token del tipo esperado.
        
        Args:
            expected_type: Tipo de token esperado
        
        Returns:
            Token consumido
        
        Raises:
            ParseError: Si el token no es del tipo esperado
        """
        token = self.current_token()
        if token is None:
            raise ParseError(f"Unexpected end of input, expected {expected_type}")
        if token.type != expected_type:
            raise ParseError(
                f"Expected {expected_type}, got {token.type} '{token.value}'",
                token
            )
        self.pos += 1
        return token
    
    def peek(self, token_type: str) -> bool:
        """
        Verifica si el token actual es del tipo dado.
        
        Args:
            token_type: Tipo a verificar
        
        Returns:
            True si coincide, False en caso contrario
        """
        token = self.current_token()
        return token is not None and token.type == token_type
    
    # ========================================================================
    # PARSING DE TÉRMINOS
    # ========================================================================
    
    def parse_term(self) -> Term:
        """
        Parsea un término Prolog.
        
        term ::= atom | variable | number | compound | list
        
        Returns:
            Término parseado
        
        Raises:
            ParseError: Si el token no corresponde a ningún término válido
        """
        token = self.current_token()
        
        if token is None:
            raise ParseError("Unexpected end of input while parsing term")
        
        # Variable
        if token.type == 'VAR':
            self.consume('VAR')
            return Variable(token.value)
        
        # Número
        if token.type == 'NUMBER':
            self.consume('NUMBER')
            if '.' in token.value:
                return Number(float(token.value))
            return Number(int(token.value))
        
        # Lista
        if token.type == 'LBRACKET':
            return self.parse_list()
        
        # Átomo o compuesto
        if token.type == 'ATOM':
            atom_token = self.consume('ATOM')
            
            # Si sigue '(', es compuesto
            if self.peek('LPAREN'):
                return self.parse_compound(atom_token.value)
            
            # Solo átomo
            return Atom(atom_token.value)
        
        raise ParseError(f"Unexpected token {token.type} '{token.value}'", token)
    
    def parse_compound(self, functor: str) -> Compound:
        """
        Parsea un término compuesto.
        
        compound ::= atom '(' args ')'
        args     ::= term (',' term)*
        
        Args:
            functor: Nombre del functor (ya consumido)
        
        Returns:
            Compound term
        """
        self.consume('LPAREN')
        
        # Parsear argumentos
        args = []
        args.append(self.parse_term())
        
        while self.peek('COMMA'):
            self.consume('COMMA')
            args.append(self.parse_term())
        
        self.consume('RPAREN')
        
        return Compound(functor, tuple(args))
    
    def parse_list(self) -> Term:
        """
        Parsea una lista Prolog.
        
        list       ::= '[' ']' | '[' list_items ']'
        list_items ::= term (',' term)* ('|' term)?
        
        Returns:
            Term representando la lista
        """
        self.consume('LBRACKET')
        
        # Lista vacía
        if self.peek('RBRACKET'):
            self.consume('RBRACKET')
            return Atom('[]')
        
        # Parsear elementos
        elements = []
        elements.append(self.parse_term())
        
        tail = None
        
        while self.peek('COMMA'):
            self.consume('COMMA')
            elements.append(self.parse_term())
        
        # Cola [H|T]
        if self.peek('PIPE'):
            self.consume('PIPE')
            tail = self.parse_term()
        
        self.consume('RBRACKET')
        
        return ListTerm(tuple(elements), tail).to_compound()
    
    # ========================================================================
    # PARSING DE CLÁUSULAS Y PROGRAMAS
    # ========================================================================
    
    def parse_clause(self) -> Clause:
        """
        Parsea una cláusula (hecho o regla).
        
        clause ::= fact | rule
        fact   ::= term '.'
        rule   ::= term ':-' body '.'
        
        Returns:
            Clause
        
        Raises:
            ParseError: Si la cabeza no es un Compound
        """
        head = self.parse_term()
        
        if not isinstance(head, Compound):
            raise ParseError(
                f"Clause head must be compound, got {type(head).__name__}"
            )
        
        # Regla
        if self.peek('IMPLIES'):
            self.consume('IMPLIES')
            body = self.parse_body()
            self.consume('DOT')
            return Clause(head, body)
        
        # Hecho
        self.consume('DOT')
        return Clause(head, None)
    
    def parse_body(self) -> List[Compound]:
        """
        Parsea el cuerpo de una regla.
        
        body ::= goal (',' goal)*
        goal ::= term
        
        Returns:
            Lista de goals
        
        Raises:
            ParseError: Si un goal no es Compound
        """
        goals = []
        goal = self.parse_term()
        
        if not isinstance(goal, Compound):
            raise ParseError(
                f"Goal must be compound, got {type(goal).__name__}"
            )
        goals.append(goal)
        
        while self.peek('COMMA'):
            self.consume('COMMA')
            goal = self.parse_term()
            if not isinstance(goal, Compound):
                raise ParseError(
                    f"Goal must be compound, got {type(goal).__name__}"
                )
            goals.append(goal)
        
        return goals
    
    def parse_query(self) -> Query:
        """
        Parsea una consulta.
        
        query ::= '?-' body '.'
        
        Returns:
            Query
        """
        self.consume('QUERY')
        goals = self.parse_body()
        self.consume('DOT')
        return Query(goals)
    
    def parse_program(self) -> Program:
        """
        Parsea un programa completo.
        
        program ::= clause*
        
        Returns:
            Program
        """
        clauses = []
        
        while self.current_token() is not None:
            # Saltar consultas en el programa
            if self.peek('QUERY'):
                self.parse_query()
                continue
            
            clauses.append(self.parse_clause())
        
        return Program(clauses)


# ============================================================================
# API SIMPLIFICADA
# ============================================================================

def parse(text: str) -> Program:
    """
    Parsea un programa Prolog completo.
    
    Args:
        text: Código Prolog como string
    
    Returns:
        Programa parseado
    
    Example:
        >>> program = parse('''
        ...     padre(juan, maria).
        ...     padre(juan, pedro).
        ...     abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        ... ''')
        >>> print(len(program.clauses))
        3
    """
    lexer = Lexer(text)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    return parser.parse_program()


def parse_query(text: str) -> Query:
    """
    Parsea una consulta Prolog.
    
    Args:
        text: Consulta como string
    
    Returns:
        Query parseada
    
    Example:
        >>> query = parse_query("?- padre(X, maria).")
        >>> print(query.goals[0])
        padre(X, maria)
    """
    lexer = Lexer(text)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    return parser.parse_query()


# ============================================================================
# TESTS
# ============================================================================

def test_parser():
    """Tests del parser"""
    
    print("=" * 60)
    print("TESTS DEL PARSER")
    print("=" * 60)
    
    # Test 1: Hechos simples
    print("\n1. Hechos simples:")
    prog = parse("padre(juan, maria). madre(ana, maria).")
    print(prog)
    assert len(prog.clauses) == 2
    print("   ✓ 2 cláusulas parseadas")
    
    # Test 2: Regla simple
    print("\n2. Regla simple:")
    prog = parse("abuelo(X, Z) :- padre(X, Y), padre(Y, Z).")
    print(prog)
    assert len(prog.clauses) == 1
    assert prog.clauses[0].is_rule()
    print("   ✓ Regla parseada correctamente")
    
    # Test 3: Listas
    print("\n3. Listas:")
    prog = parse("lista([1, 2, 3]). cons([H|T]).")
    print(prog)
    assert len(prog.clauses) == 2
    print("   ✓ Listas parseadas")
    
    # Test 4: Consulta
    print("\n4. Consulta:")
    query = parse_query("?- padre(X, maria).")
    print(query)
    assert len(query.goals) == 1
    print("   ✓ Consulta parseada")
    
    # Test 5: Programa completo
    print("\n5. Programa completo:")
    code = """
        % Hechos
        padre(juan, maria).
        padre(pedro, juan).
        
        % Reglas
        abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
    """
    prog = parse(code)
    print(prog)
    assert len(prog.clauses) == 3
    print("   ✓ Programa completo parseado")
    
    # Test 6: Error de sintaxis
    print("\n6. Error de sintaxis:")
    try:
        parse("padre(juan")
    except ParseError as e:
        print(f"   ✓ Error capturado: {e}")
    
    print("\n" + "=" * 60)
    print("TODOS LOS TESTS PASARON ✓")
    print("=" * 60)


if __name__ == "__main__":
    test_parser()