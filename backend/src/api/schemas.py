"""
schemas.py - Modelos Pydantic para la API

Define los esquemas de request/response para validación automática.
"""

from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any


# ============================================================================
# REQUEST MODELS
# ============================================================================

class ParseRequest(BaseModel):
    """
    Request para parsear código Prolog.
    
    Attributes:
        code: Código Prolog a parsear
    """
    code: str = Field(
        ...,
        description="Código Prolog a parsear",
        min_length=1,
        example="padre(juan, maria)."
    )
    
    model_config = {
        "json_schema_extra": {
            "examples": [{
                "code": "padre(juan, maria). madre(ana, maria)."
            }]
        }
    }


class ResolveRequest(BaseModel):
    """
    Request para resolver una consulta Prolog.
    
    Attributes:
        program: Programa Prolog (hechos y reglas)
        query: Consulta Prolog
        strategy: Estrategia de selección ("leftmost" o "rightmost")
        max_depth: Profundidad máxima del árbol
    """
    program: str = Field(
        ...,
        description="Programa Prolog (hechos y reglas)",
        min_length=1
    )
    query: str = Field(
        ...,
        description="Consulta Prolog (ej: ?- padre(X, maria).)",
        min_length=4
    )
    strategy: str = Field(
        default="leftmost",
        description="Estrategia de selección de goals",
        pattern="^(leftmost|rightmost)$"
    )
    max_depth: int = Field(
        default=20,
        description="Profundidad máxima del árbol (prevenir loops infinitos)",
        ge=1,
        le=100
    )
    
    model_config = {
        "json_schema_extra": {
            "examples": [{
                "program": """padre(juan, maria).
padre(juan, pedro).
abuelo(X, Z) :- padre(X, Y), padre(Y, Z).""",
                "query": "?- abuelo(juan, Z).",
                "strategy": "leftmost",
                "max_depth": 20
            }]
        }
    }


# ============================================================================
# RESPONSE MODELS
# ============================================================================

class ParseResponse(BaseModel):
    """
    Response del parser.
    
    Attributes:
        success: Indica si el parsing fue exitoso
        clauses: Lista de cláusulas parseadas (strings)
        num_clauses: Número de cláusulas encontradas
        error: Mensaje de error si falló
    """
    success: bool
    clauses: Optional[List[str]] = None
    num_clauses: int = 0
    error: Optional[str] = None
    
    model_config = {
        "json_schema_extra": {
            "examples": [
                {
                    "success": True,
                    "clauses": [
                        "padre(juan, maria).",
                        "madre(ana, maria)."
                    ],
                    "num_clauses": 2,
                    "error": None
                },
                {
                    "success": False,
                    "clauses": None,
                    "num_clauses": 0,
                    "error": "Parse error: Expected DOT, got EOF at line 1"
                }
            ]
        }
    }


class SolutionResponse(BaseModel):
    """
    Representa una solución encontrada.
    
    Attributes:
        node_id: ID del nodo exitoso
        substitution: Sustitución en formato string
        goals: Goals del nodo (vacío para éxito)
    """
    node_id: str = Field(..., description="ID del nodo de solución")
    substitution: str = Field(..., description="Sustitución encontrada")
    goals: List[str] = Field(default_factory=list, description="Goals restantes")
    
    model_config = {
        "json_schema_extra": {
            "examples": [{
                "node_id": "a1b2c3d4",
                "substitution": "{X ↦ maria, Y ↦ pedro}",
                "goals": []
            }]
        }
    }


class ResolveResponse(BaseModel):
    """
    Response de la resolución SLD.
    
    Attributes:
        success: Indica si la resolución fue exitosa
        tree: Árbol SLD completo (diccionario serializado)
        solutions: Lista de soluciones encontradas
        stats: Estadísticas del árbol
        error: Mensaje de error si falló
    """
    success: bool
    tree: Optional[Dict[str, Any]] = None
    solutions: Optional[List[SolutionResponse]] = None
    stats: Optional[Dict[str, int]] = None
    error: Optional[str] = None
    
    model_config = {
        "json_schema_extra": {
            "examples": [{
                "success": True,
                "tree": {
                    "root": "abc123",
                    "nodes": [],
                    "solutions": []
                },
                "solutions": [
                    {
                        "node_id": "abc123",
                        "substitution": "{X ↦ maria}",
                        "goals": []
                    }
                ],
                "stats": {
                    "total_nodes": 5,
                    "solutions_found": 2,
                    "max_depth_reached": 3,
                    "success_nodes": 2,
                    "failure_nodes": 2
                },
                "error": None
            }]
        }
    }


# ============================================================================
# VALIDATION HELPERS
# ============================================================================

def validate_prolog_query(query: str) -> bool:
    """
    Valida básicamente que una consulta tenga formato correcto.
    
    Args:
        query: String de consulta
    
    Returns:
        True si parece válida
    """
    query = query.strip()
    return query.startswith("?-") and query.endswith(".")


def validate_strategy(strategy: str) -> bool:
    """
    Valida que la estrategia sea válida.
    
    Args:
        strategy: Nombre de estrategia
    
    Returns:
        True si es válida
    """
    return strategy in ["leftmost", "rightmost"]