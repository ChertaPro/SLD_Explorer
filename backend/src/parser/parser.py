"""
parser.py - Parser de Prolog Simplificado

Parser recursivo descendente para un subconjunto de Prolog:
- Hechos: padre(juan, maria).
- Reglas: abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
- Consultas: ?- padre(X, maria).
- Listas: [1,2,3], [H|T]
- Números y átomos

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

import re
from typing import List, Optional, Tuple, Union
from dataclasses import dataclass
from .terms import (
    Term, Variable, Atom, Number, Compound, 
    List as ListTerm, Substitution
)


# ============================================================================
# TOKENIZACIÓN
# ============================================================================

@dataclass
class Token:
    """Representa un token del lexer"""
    type: str  # ATOM, VAR, NUMBER, LPAREN, etc.
    value: str
    line: int
    column: int


class Lexer:
    """
    Tokenizador para Prolog.
    
    Tipos de tokens:
    - ATOM: juan, padre, rojo
    - VAR: X, Y, _result
    - NUMBER: 42, 3.14
    - LPAREN, RPAREN: (, )
    - LBRACKET, RBRACKET: [, ]
    - DOT: .
    - COMMA: ,
    - PIPE: |
    - IMPLIES: :-
    - QUERY: ?-
    """
    
    TOKEN_PATTERNS = [
        ('QUERY', r'\?-'),
        ('IMPLIES', r':-'),
        ('NUMBER', r'-?\d+\.?\d*'),
        ('VAR', r'[A-Z_][a-zA-Z0-9_]*'),
        ('ATOM', r'[a-z][a-zA-Z0-9_]*'),
        ('QUOTED_ATOM', r"'([^'\\\\]|\\\\.)*'"),
        ('LPAREN', r'\('),
        ('RPAREN', r'\)'),
        ('LBRACKET', r'\['),
        ('RBRACKET', r'\]'),
        ('DOT', r'\.'),
        ('COMMA', r','),
        ('PIPE', r'\|'),
        ('WHITESPACE', r'\s+'),
        ('COMMENT', r'%.*'),
    ]
    
    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
    
    def tokenize(self) -> List[Token]:
        """Tokeniza el texto completo"""
        while self.pos < len(self.text):
            match_found = False
            
            for token_type, pattern in self.TOKEN_PATTERNS:
                regex = re.compile(pattern)
                match = regex.match(self.text, self.pos)
                
                if match:
                    value = match.group(0)
                    
                    # Ignorar whitespace y comentarios
                    if token_type not in ('WHITESPACE', 'COMMENT'):
                        # Limpiar comillas de átomos
                        if token_type == 'QUOTED_ATOM':
                            value = value[1:-1]  # Remover comillas
                            token_type = 'ATOM'
                        
                        self.tokens.append(Token(
                            token_type, value, self.line, self.column
                        ))
                    
                    # Actualizar posición
                    self.pos = match.end()
                    
                    # Actualizar línea y columna
                    newlines = value.count('\n')
                    if newlines:
                        self.line += newlines
                        self.column = len(value.split('\n')[-1]) + 1
                    else:
                        self.column += len(value)
                    
                    match_found = True
                    break
            
            if not match_found:
                raise SyntaxError(
                    f"Invalid character '{self.text[self.pos]}' "
                    f"at line {self.line}, column {self.column}"
                )
        
        return self.tokens


# ============================================================================
# PARSER
# ============================================================================

class ParseError(Exception):
    """Excepción para errores de parsing"""
    pass


@dataclass
class Clause:
    """Representa una cláusula Prolog (hecho o regla)"""
    head: Compound
    body: Optional[List[Compound]] = None
    
    def is_fact(self) -> bool:
        return self.body is None
    
    def is_rule(self) -> bool:
        return self.body is not None
    
    def __str__(self) -> str:
        if self.is_fact():
            return f"{self.head}."
        body_str = ", ".join(str(g) for g in self.body)
        return f"{self.head} :- {body_str}."


@dataclass
class Query:
    """Representa una consulta Prolog"""
    goals: List[Compound]
    
    def __str__(self) -> str:
        goals_str = ", ".join(str(g) for g in self.goals)
        return f"?- {goals_str}."


@dataclass
class Program:
    """Representa un programa Prolog completo"""
    clauses: List[Clause]
    
    def __str__(self) -> str:
        return "\n".join(str(c) for c in self.clauses)


class Parser:
    """
    Parser recursivo descendente para Prolog.
    """
    
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
    
    def current_token(self) -> Optional[Token]:
        """Retorna el token actual sin consumirlo"""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None
    
    def consume(self, expected_type: str) -> Token:
        """Consume un token del tipo esperado"""
        token = self.current_token()
        if token is None:
            raise ParseError(f"Unexpected end of input, expected {expected_type}")
        if token.type != expected_type:
            raise ParseError(
                f"Expected {expected_type}, got {token.type} '{token.value}' "
                f"at line {token.line}, column {token.column}"
            )
        self.pos += 1
        return token
    
    def peek(self, token_type: str) -> bool:
        """Verifica si el token actual es del tipo dado"""
        token = self.current_token()
        return token is not None and token.type == token_type
    
    # ========================================================================
    # PARSING DE TÉRMINOS
    # ========================================================================
    
    def parse_term(self) -> Term:
        """
        term ::= atom | variable | number | compound | list
        """
        token = self.current_token()
        
        if token is None:
            raise ParseError("Unexpected end of input")
        
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
        
        raise ParseError(f"Unexpected token {token.type} '{token.value}'")
    
    def parse_compound(self, functor: str) -> Compound:
        """
        compound ::= atom '(' args ')'
        args     ::= term (',' term)*
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
        list       ::= '[' ']' | '[' list_items ']'
        list_items ::= term (',' term)* ('|' term)?
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
    # PARSING DE CLÁUSULAS
    # ========================================================================
    
    def parse_clause(self) -> Clause:
        """
        clause ::= fact | rule
        fact   ::= term '.'
        rule   ::= term ':-' body '.'
        """
        head = self.parse_term()
        
        if not isinstance(head, Compound):
            raise ParseError(f"Clause head must be compound, got {type(head)}")
        
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
        body ::= goal (',' goal)*
        goal ::= term
        """
        goals = []
        goals.append(self.parse_term())
        
        if not isinstance(goals[0], Compound):
            raise ParseError(f"Goal must be compound, got {type(goals[0])}")
        
        while self.peek('COMMA'):
            self.consume('COMMA')
            goal = self.parse_term()
            if not isinstance(goal, Compound):
                raise ParseError(f"Goal must be compound, got {type(goal)}")
            goals.append(goal)
        
        return goals
    
    def parse_query(self) -> Query:
        """
        query ::= '?-' body '.'
        """
        self.consume('QUERY')
        goals = self.parse_body()
        self.consume('DOT')
        return Query(goals)
    
    def parse_program(self) -> Program:
        """
        program ::= clause*
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
    
    Ejemplo:
        program = parse('''
            padre(juan, maria).
            padre(juan, pedro).
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        ''')
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
    
    Ejemplo:
        query = parse_query("?- padre(X, maria).")
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
    
    # Test 2: Regla simple
    print("\n2. Regla simple:")
    prog = parse("abuelo(X, Z) :- padre(X, Y), padre(Y, Z).")
    print(prog)
    
    # Test 3: Listas
    print("\n3. Listas:")
    prog = parse("lista([1, 2, 3]). cons([H|T]).")
    print(prog)
    
    # Test 4: Consulta
    print("\n4. Consulta:")
    query = parse_query("?- padre(X, maria).")
    print(query)
    
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
    
    print("\n" + "=" * 60)


if __name__ == "__main__":
    test_parser()