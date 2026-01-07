"""
main.py - API REST para SLD-Explorer

Endpoints:
- POST /api/resolve: Resuelve una consulta y retorna el árbol SLD
- POST /api/resolve-step: Resuelve paso a paso (streaming)
- POST /api/parse: Parsea código Prolog y retorna el AST
- GET /api/health: Health check
"""

from fastapi import FastAPI, HTTPException, WebSocket
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
import json
import asyncio

from .parser import parse, parse_query, ParseError, Program, Query, Clause
from .sld import SLDResolver, SLDTree, SLDNode
from .parser.terms import Compound


# ============================================================================
# CONFIGURACIÓN DE LA APP
# ============================================================================

app = FastAPI(
    title="SLD-Explorer API",
    description="API para visualización de árboles SLD de Prolog",
    version="1.0.0"
)

# CORS para permitir requests desde el frontend
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # En producción, especificar dominios
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# ============================================================================
# MODELOS PYDANTIC
# ============================================================================

class ParseRequest(BaseModel):
    """Request para parsear código Prolog"""
    code: str = Field(..., description="Código Prolog a parsear")
    
    class Config:
        json_schema_extra = {
            "example": {
                "code": "padre(juan, maria). madre(ana, maria)."
            }
        }


class ResolveRequest(BaseModel):
    """Request para resolver una consulta"""
    program: str = Field(..., description="Programa Prolog (hechos y reglas)")
    query: str = Field(..., description="Consulta Prolog (ej: ?- padre(X, maria).)")
    strategy: str = Field(default="leftmost", description="Estrategia de selección")
    max_depth: int = Field(default=20, description="Profundidad máxima del árbol")
    
    class Config:
        json_schema_extra = {
            "example": {
                "program": """
                    padre(juan, maria).
                    padre(juan, pedro).
                    abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
                """,
                "query": "?- abuelo(juan, Z).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        }


class ParseResponse(BaseModel):
    """Response del parser"""
    success: bool
    clauses: Optional[List[str]] = None
    error: Optional[str] = None
    
    class Config:
        json_schema_extra = {
            "example": {
                "success": True,
                "clauses": [
                    "padre(juan, maria).",
                    "madre(ana, maria)."
                ],
                "error": None
            }
        }


class SolutionResponse(BaseModel):
    """Representa una solución encontrada"""
    node_id: str
    substitution: str
    goals: List[str]


class ResolveResponse(BaseModel):
    """Response de la resolución"""
    success: bool
    tree: Optional[Dict[str, Any]] = None
    solutions: Optional[List[SolutionResponse]] = None
    stats: Optional[Dict[str, int]] = None
    error: Optional[str] = None


# ============================================================================
# ENDPOINTS
# ============================================================================

@app.get("/api/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "service": "SLD-Explorer API",
        "version": "1.0.0"
    }


@app.post("/api/parse", response_model=ParseResponse)
async def parse_code(request: ParseRequest):
    """
    Parsea código Prolog y retorna las cláusulas.
    
    Este endpoint es útil para validar sintaxis antes de resolver.
    """
    try:
        program = parse(request.code)
        
        return ParseResponse(
            success=True,
            clauses=[str(clause) for clause in program.clauses],
            error=None
        )
    
    except ParseError as e:
        return ParseResponse(
            success=False,
            clauses=None,
            error=str(e)
        )
    
    except Exception as e:
        return ParseResponse(
            success=False,
            clauses=None,
            error=f"Unexpected error: {str(e)}"
        )


@app.post("/api/resolve", response_model=ResolveResponse)
async def resolve_query(request: ResolveRequest):
    """
    Resuelve una consulta Prolog y retorna el árbol SLD completo.
    
    Este endpoint construye el árbol completo de una vez.
    Para visualización paso a paso, usar WebSocket.
    """
    try:
        # 1. Parsear programa
        program = parse(request.program)
        
        # 2. Parsear consulta
        query = parse_query(request.query)
        
        # 3. Crear resolver
        resolver = SLDResolver(
            program=program,
            max_depth=request.max_depth
        )
        
        # 4. Resolver consulta
        tree = resolver.resolve(
            query_goals=query.goals,
            strategy=request.strategy
        )
        
        # 5. Extraer soluciones
        solutions = tree.find_solutions()
        solutions_response = [
            SolutionResponse(
                node_id=node.node_id,
                substitution=str(subst),
                goals=[str(g) for g in node.goals]
            )
            for node, subst in solutions
        ]
        
        # 6. Estadísticas
        stats = {
            "total_nodes": len(tree.all_nodes),
            "solutions_found": len(solutions),
            "max_depth_reached": max(node.depth for node in tree.all_nodes)
        }
        
        return ResolveResponse(
            success=True,
            tree=tree.to_dict(),
            solutions=solutions_response,
            stats=stats,
            error=None
        )
    
    except ParseError as e:
        raise HTTPException(
            status_code=400,
            detail=f"Parse error: {str(e)}"
        )
    
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Resolution error: {str(e)}"
        )


@app.websocket("/ws/resolve-step")
async def resolve_step_websocket(websocket: WebSocket):
    """
    WebSocket endpoint para resolución paso a paso.
    
    El cliente envía:
    {
        "program": "...",
        "query": "...",
        "strategy": "leftmost",
        "max_depth": 20
    }
    
    El servidor envía updates con cada paso:
    {
        "type": "step",
        "tree": {...},
        "current_node": {...}
    }
    
    Al finalizar:
    {
        "type": "complete",
        "solutions": [...]
    }
    """
    await websocket.accept()
    
    try:
        # Recibir configuración
        data = await websocket.receive_json()
        
        # Parsear
        program = parse(data["program"])
        query = parse_query(data["query"])
        strategy = data.get("strategy", "leftmost")
        max_depth = data.get("max_depth", 20)
        
        # Crear resolver
        resolver = SLDResolver(program=program, max_depth=max_depth)
        
        # Resolver paso a paso
        step_num = 0
        for tree, current_node in resolver.resolve_step_by_step(
            query_goals=query.goals,
            strategy=strategy
        ):
            step_num += 1
            
            # Enviar update
            await websocket.send_json({
                "type": "step",
                "step": step_num,
                "tree": tree.to_dict(),
                "current_node": current_node.to_dict(),
                "status": current_node.status.value
            })
            
            # Pequeña pausa para visualización
            await asyncio.sleep(0.1)
        
        # Enviar resultado final
        solutions = tree.find_solutions()
        await websocket.send_json({
            "type": "complete",
            "solutions": [
                {
                    "node_id": node.node_id,
                    "substitution": str(subst)
                }
                for node, subst in solutions
            ],
            "stats": {
                "total_nodes": len(tree.all_nodes),
                "solutions_found": len(solutions),
                "total_steps": step_num
            }
        })
        
    except Exception as e:
        await websocket.send_json({
            "type": "error",
            "error": str(e)
        })
    
    finally:
        await websocket.close()


@app.post("/api/validate")
async def validate_syntax(request: ParseRequest):
    """
    Valida la sintaxis de código Prolog sin resolverlo.
    
    Útil para feedback en tiempo real en el editor.
    """
    try:
        parse(request.code)
        return {
            "valid": True,
            "error": None
        }
    except ParseError as e:
        return {
            "valid": False,
            "error": str(e)
        }


# ============================================================================
# EJEMPLOS Y DOCUMENTACIÓN
# ============================================================================

@app.get("/api/examples")
async def get_examples():
    """
    Retorna ejemplos de programas Prolog para el frontend.
    """
    return {
        "examples": [
            {
                "name": "Relaciones Familiares",
                "program": """
% Hechos
padre(juan, maria).
padre(juan, pedro).
padre(pedro, ana).
madre(ana, maria).

% Reglas
abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
hermano(X, Y) :- padre(P, X), padre(P, Y).
                """,
                "queries": [
                    "?- padre(juan, X).",
                    "?- abuelo(juan, Z).",
                    "?- hermano(maria, pedro)."
                ]
            },
            {
                "name": "Listas",
                "program": """
% Concatenar listas
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Longitud de lista
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Miembro de lista
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
                """,
                "queries": [
                    "?- append([1,2], [3,4], X).",
                    "?- member(2, [1,2,3]).",
                    "?- length([a,b,c], N)."
                ]
            },
            {
                "name": "Números",
                "program": """
% Factorial
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

% Fibonacci
fib(0, 0).
fib(1, 1).
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.
                """,
                "queries": [
                    "?- factorial(5, X).",
                    "?- fib(6, X)."
                ]
            }
        ]
    }


# ============================================================================
# MAIN
# ============================================================================

if __name__ == "__main__":
    import uvicorn
    
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )