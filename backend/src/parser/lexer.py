"""
lexer.py - Tokenizador para Prolog

Responsabilidades:
- Convertir texto plano en tokens
- Manejo de whitespace y comentarios
- Validación léxica básica
"""

import re
from dataclasses import dataclass
from typing import List


@dataclass
class Token:
    """
    Representa un token del lexer.
    
    Attributes:
        type: Tipo de token (ATOM, VAR, NUMBER, etc.)
        value: Valor del token
        line: Línea donde aparece
        column: Columna donde aparece
    """
    type: str
    value: str
    line: int
    column: int
    
    def __repr__(self) -> str:
        return f"Token({self.type}, '{self.value}', {self.line}:{self.column})"


class LexerError(Exception):
    """Excepción para errores léxicos"""
    pass


class Lexer:
    """
    Tokenizador para Prolog.
    
    Tipos de tokens reconocidos:
    - ATOM: juan, padre, rojo
    - VAR: X, Y, _result
    - NUMBER: 42, 3.14, -5
    - LPAREN, RPAREN: (, )
    - LBRACKET, RBRACKET: [, ]
    - DOT: .
    - COMMA: ,
    - PIPE: |
    - IMPLIES: :-
    - QUERY: ?-
    - COMMENT: % comentario
    """
    
    # Patrones de tokens en orden de prioridad
    TOKEN_PATTERNS = [
        ('QUERY', r'\?-'),
        ('IMPLIES', r':-'),
        ('NUMBER', r'-?\d+\.?\d*'),
        ('VAR', r'[A-Z_][a-zA-Z0-9_]*'),
        ('ATOM', r'[a-z][a-zA-Z0-9_]*'),
        ('QUOTED_ATOM', r"'([^'\\]|\\.)*'"),
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
        """
        Inicializa el lexer.
        
        Args:
            text: Código Prolog a tokenizar
        """
        self.text = text
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
    
    def tokenize(self) -> List[Token]:
        """
        Tokeniza el texto completo.
        
        Returns:
            Lista de tokens encontrados
        
        Raises:
            LexerError: Si encuentra un carácter inválido
        """
        while self.pos < len(self.text):
            match_found = False
            
            for token_type, pattern in self.TOKEN_PATTERNS:
                regex = re.compile(pattern)
                match = regex.match(self.text, self.pos)
                
                if match:
                    value = match.group(0)
                    
                    # Ignorar whitespace y comentarios
                    if token_type not in ('WHITESPACE', 'COMMENT'):
                        # Limpiar comillas de átomos quoted
                        if token_type == 'QUOTED_ATOM':
                            value = value[1:-1]  # Remover comillas
                            token_type = 'ATOM'
                        
                        self.tokens.append(Token(
                            type=token_type,
                            value=value,
                            line=self.line,
                            column=self.column
                        ))
                    
                    # Actualizar posición
                    self._advance(match.end() - self.pos, value)
                    match_found = True
                    break
            
            if not match_found:
                raise LexerError(
                    f"Invalid character '{self.text[self.pos]}' "
                    f"at line {self.line}, column {self.column}"
                )
        
        return self.tokens
    
    def _advance(self, length: int, value: str) -> None:
        """
        Avanza la posición del lexer y actualiza línea/columna.
        
        Args:
            length: Número de caracteres a avanzar
            value: Valor del token (para contar newlines)
        """
        self.pos += length
        
        # Actualizar línea y columna
        newlines = value.count('\n')
        if newlines:
            self.line += newlines
            self.column = len(value.split('\n')[-1]) + 1
        else:
            self.column += len(value)
    
    def peek_token(self, offset: int = 0) -> Token | None:
        """
        Mira un token sin consumirlo.
        
        Args:
            offset: Posición relativa (0 = actual, 1 = siguiente, etc.)
        
        Returns:
            Token en la posición especificada o None
        """
        if offset < len(self.tokens):
            return self.tokens[offset]
        return None


# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

def tokenize(text: str) -> List[Token]:
    """
    Función de conveniencia para tokenizar texto.
    
    Args:
        text: Código Prolog
    
    Returns:
        Lista de tokens
    
    Example:
        >>> tokens = tokenize("padre(juan, maria).")
        >>> print(tokens[0])
        Token(ATOM, 'padre', 1:1)
    """
    lexer = Lexer(text)
    return lexer.tokenize()


# ============================================================================
# TESTS
# ============================================================================

def test_lexer():
    """Tests del lexer"""
    
    print("=" * 60)
    print("TESTS DEL LEXER")
    print("=" * 60)
    
    # Test 1: Tokens básicos
    print("\n1. Tokens básicos:")
    tokens = tokenize("padre(juan, maria).")
    for token in tokens:
        print(f"   {token}")
    
    # Test 2: Variables y números
    print("\n2. Variables y números:")
    tokens = tokenize("suma(X, 42, Y).")
    for token in tokens:
        print(f"   {token}")
    
    # Test 3: Listas
    print("\n3. Listas:")
    tokens = tokenize("[1, 2, 3]")
    for token in tokens:
        print(f"   {token}")
    
    # Test 4: Consulta
    print("\n4. Consulta:")
    tokens = tokenize("?- padre(X, maria).")
    for token in tokens:
        print(f"   {token}")
    
    # Test 5: Comentarios (deben ignorarse)
    print("\n5. Comentarios (deben ignorarse):")
    tokens = tokenize("padre(juan, maria). % Este es un comentario")
    print(f"   Total tokens (sin comentarios): {len(tokens)}")
    for token in tokens:
        print(f"   {token}")
    
    # Test 6: Error léxico
    print("\n6. Error léxico:")
    try:
        tokens = tokenize("padre@juan")
    except LexerError as e:
        print(f"   ✓ Error capturado: {e}")
    
    print("\n" + "=" * 60)


if __name__ == "__main__":
    test_lexer()