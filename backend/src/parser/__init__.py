"""Parser module - Lexer, Parser, and Term representation."""

# Terms
from .terms import (
    Term,
    Variable,
    Atom,
    Number,
    Compound,
    List,
    Substitution,
)

# Lexer
from .lexer import (
    Token,
    Lexer,
    LexerError,
    tokenize,
)

# Parser
from .parser import (
    ParseError,
    Clause,
    Query,
    Program,
    Parser,
    parse,
    parse_query,
)

__all__ = [
    # Terms
    "Term",
    "Variable",
    "Atom",
    "Number",
    "Compound",
    "List",
    "Substitution",
    # Lexer
    "Token",
    "Lexer",
    "LexerError",
    "tokenize",
    # Parser
    "ParseError",
    "Clause",
    "Query",
    "Program",
    "Parser",
    "parse",
    "parse_query",
]