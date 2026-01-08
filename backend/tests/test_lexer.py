"""
test_lexer.py - Tests para el tokenizador

Tests esenciales:
- Tokenización correcta de diferentes tipos
- Manejo de whitespace y comentarios
- Detección de errores léxicos
"""

import pytest
from src.parser.lexer import Lexer, Token, LexerError, tokenize


class TestBasicTokenization:
    """Tests de tokenización básica"""
    
    def test_atoms(self):
        """Test tokenización de átomos"""
        tokens = tokenize("padre juan maria")
        assert len(tokens) == 3
        assert all(t.type == "ATOM" for t in tokens)
        assert [t.value for t in tokens] == ["padre", "juan", "maria"]
    
    def test_variables(self):
        """Test tokenización de variables"""
        tokens = tokenize("X Y Z _result")
        assert len(tokens) == 4
        assert all(t.type == "VAR" for t in tokens)
        assert [t.value for t in tokens] == ["X", "Y", "Z", "_result"]
    
    def test_numbers(self):
        """Test tokenización de números"""
        tokens = tokenize("42 3.14 -5")
        assert len(tokens) == 3
        assert all(t.type == "NUMBER" for t in tokens)
        assert [t.value for t in tokens] == ["42", "3.14", "-5"]
    
    def test_special_tokens(self):
        """Test tokenización de símbolos especiales"""
        code = "?- :- . , | ( ) [ ]"
        tokens = tokenize(code)
        expected_types = ["QUERY", "IMPLIES", "DOT", "COMMA", "PIPE", 
                         "LPAREN", "RPAREN", "LBRACKET", "RBRACKET"]
        assert [t.type for t in tokens] == expected_types


class TestComplexExpressions:
    """Tests de expresiones complejas"""
    
    def test_simple_fact(self):
        """Test: padre(juan, maria)."""
        tokens = tokenize("padre(juan, maria).")
        types = [t.type for t in tokens]
        assert types == ["ATOM", "LPAREN", "ATOM", "COMMA", "ATOM", "RPAREN", "DOT"]
    
    def test_rule(self):
        """Test: abuelo(X, Z) :- padre(X, Y)."""
        tokens = tokenize("abuelo(X, Z) :- padre(X, Y).")
        types = [t.type for t in tokens]
        assert "ATOM" in types
        assert "VAR" in types
        assert "IMPLIES" in types
        assert "DOT" in types
    
    def test_list(self):
        """Test: [1, 2, 3]"""
        tokens = tokenize("[1, 2, 3]")
        types = [t.type for t in tokens]
        assert types == ["LBRACKET", "NUMBER", "COMMA", "NUMBER", 
                        "COMMA", "NUMBER", "RBRACKET"]
    
    def test_list_with_tail(self):
        """Test: [H|T]"""
        tokens = tokenize("[H|T]")
        types = [t.type for t in tokens]
        assert types == ["LBRACKET", "VAR", "PIPE", "VAR", "RBRACKET"]


class TestWhitespaceAndComments:
    """Tests de manejo de whitespace y comentarios"""
    
    def test_whitespace_ignored(self):
        """Whitespace debe ser ignorado"""
        tokens1 = tokenize("padre(juan,maria)")
        tokens2 = tokenize("padre( juan , maria )")
        assert len(tokens1) == len(tokens2)
    
    def test_comments_ignored(self):
        """Comentarios deben ser ignorados"""
        code = """
        padre(juan, maria). % Este es un comentario
        % Otra línea de comentario
        madre(ana, maria).
        """
        tokens = tokenize(code)
        # Solo debe haber tokens de las dos cláusulas
        assert "%" not in [t.value for t in tokens]
        atom_tokens = [t for t in tokens if t.type == "ATOM"]
        assert len(atom_tokens) == 6  # padre, juan, maria, madre, ana, maria = 6... wait
    
    def test_newlines_handled(self):
        """Saltos de línea deben manejarse correctamente"""
        code = "padre(juan).\nmadre(ana)."
        tokens = tokenize(code)
        # Verificar que line numbers son diferentes
        first_dot = [t for t in tokens if t.type == "DOT"][0]
        second_dot = [t for t in tokens if t.type == "DOT"][1]
        assert first_dot.line == 1
        assert second_dot.line == 2


class TestQuotedAtoms:
    """Tests de átomos con comillas"""
    
    def test_quoted_atom(self):
        """Test: 'hola mundo'"""
        tokens = tokenize("'hola mundo'")
        assert len(tokens) == 1
        assert tokens[0].type == "ATOM"
        assert tokens[0].value == "hola mundo"
    
    def test_quoted_atom_with_special_chars(self):
        """Test átomos con caracteres especiales"""
        tokens = tokenize("'hello, world!'")
        assert tokens[0].value == "hello, world!"


class TestErrorHandling:
    """Tests de manejo de errores"""
    
    def test_invalid_character(self):
        """Carácter inválido debe lanzar LexerError"""
        with pytest.raises(LexerError) as excinfo:
            tokenize("padre@juan")
        assert "Invalid character" in str(excinfo.value)
        assert "@" in str(excinfo.value)
    
    def test_error_includes_position(self):
        """Error debe incluir línea y columna"""
        with pytest.raises(LexerError) as excinfo:
            tokenize("padre(juan, #maria)")
        assert "line" in str(excinfo.value).lower()
        assert "column" in str(excinfo.value).lower()


class TestTokenPosition:
    """Tests de posición de tokens"""
    
    def test_single_line_positions(self):
        """Verificar posiciones en una línea"""
        tokens = tokenize("padre(juan)")
        assert tokens[0].line == 1
        assert tokens[0].column == 1  # 'padre' empieza en columna 1
    
    def test_multiline_positions(self):
        """Verificar posiciones en múltiples líneas"""
        code = "padre(juan).\nmadre(ana)."
        tokens = tokenize(code)
        
        # Primer átomo en línea 1
        assert tokens[0].line == 1
        
        # Buscar primer átomo de segunda línea
        madre_token = [t for t in tokens if t.value == "madre"][0]
        assert madre_token.line == 2


# ============================================================================
# TESTS DE INTEGRACIÓN
# ============================================================================

class TestRealWorldExamples:
    """Tests con ejemplos reales de Prolog"""
    
    def test_family_relations(self):
        """Test programa de relaciones familiares"""
        code = """
        padre(juan, maria).
        padre(pedro, juan).
        abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """
        tokens = tokenize(code)
        
        # Verificar que tokenizó correctamente
        assert len(tokens) > 0
        
        # Debe haber átomos, variables, y símbolos especiales
        types = set(t.type for t in tokens)
        assert "ATOM" in types
        assert "VAR" in types
        assert "DOT" in types
        assert "IMPLIES" in types
    
    def test_list_operations(self):
        """Test operaciones con listas"""
        code = "append([], L, L). append([H|T], L, [H|R]) :- append(T, L, R)."
        tokens = tokenize(code)
        
        # Verificar tokens de lista
        assert "LBRACKET" in [t.type for t in tokens]
        assert "RBRACKET" in [t.type for t in tokens]
        assert "PIPE" in [t.type for t in tokens]


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])