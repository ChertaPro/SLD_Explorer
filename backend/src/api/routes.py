"""
routes.py - Definición de rutas/endpoints de la API

Separa la lógica de routing de la configuración de la aplicación.
"""

from fastapi import APIRouter, HTTPException, WebSocket
import asyncio

from .schemas import (
    ParseRequest,
    ParseResponse,
    ResolveRequest,
    ResolveResponse,
    SolutionResponse,
)
from ..parser import parse, parse_query, ParseError
from ..sld import SLDResolver


# ============================================================================
# ROUTER PRINCIPAL
# ============================================================================

router = APIRouter(prefix="/api", tags=["sld-explorer"])


# ============================================================================
# ENDPOINTS DE HEALTH Y UTILIDAD
# ============================================================================

@router.get("/health")
async def health_check():
    """
    Health check endpoint.
    
    Returns:
        Estado del servicio
    """
    return {
        "status": "healthy",
        "service": "SLD-Explorer API",
        "version": "1.0.0"
    }


@router.get("/examples")
async def get_examples():
    """
    Retorna ejemplos de programas Prolog para el frontend.
    
    Returns:
        Lista de ejemplos con programa y consultas sugeridas
    """
    return {
        "examples": [
            {
                "name": "Relaciones Familiares",
                "description": "Ejemplo básico de hechos y reglas",
                "program": """% Hechos
padre(juan, maria).
padre(juan, pedro).
padre(pedro, ana).
madre(ana, maria).

% Reglas
abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
hermano(X, Y) :- padre(P, X), padre(P, Y).""",
                "queries": [
                    "?- padre(juan, X).",
                    "?- abuelo(juan, Z).",
                    "?- hermano(maria, pedro)."
                ]
            },
            {
                "name": "Listas - Append",
                "description": "Concatenación de listas",
                "program": """% Concatenar listas
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).""",
                "queries": [
                    "?- append([1,2], [3,4], X).",
                    "?- append(X, Y, [1,2,3]).",
                ]
            },
            {
                "name": "Miembro de Lista",
                "description": "Verificar pertenencia en listas",
                "program": """% Verificar pertenencia
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).""",
                "queries": [
                    "?- member(2, [1,2,3]).",
                    "?- member(X, [a,b,c]).",
                ]
            }
        ]
    }


# ============================================================================
# ENDPOINTS DE PARSING
# ============================================================================

@router.post("/parse", response_model=ParseResponse)
async def parse_code(request: ParseRequest):
    """
    Parsea código Prolog y retorna las cláusulas.
    
    Este endpoint es útil para:
    - Validar sintaxis antes de resolver
    - Mostrar estructura del programa
    - Debugging
    
    Args:
        request: Código Prolog a parsear
    
    Returns:
        Lista de cláusulas o error
    """
    try:
        program = parse(request.code)
        
        return ParseResponse(
            success=True,
            clauses=[str(clause) for clause in program.clauses],
            num_clauses=len(program.clauses),
            error=None
        )
    
    except ParseError as e:
        return ParseResponse(
            success=False,
            clauses=None,
            num_clauses=0,
            error=str(e)
        )
    
    except Exception as e:
        return ParseResponse(
            success=False,
            clauses=None,
            num_clauses=0,
            error=f"Unexpected error: {str(e)}"
        )


@router.post("/validate")
async def validate_syntax(request: ParseRequest):
    """
    Valida la sintaxis de código Prolog sin resolverlo.
    
    Útil para feedback en tiempo real en el editor.
    
    Args:
        request: Código a validar
    
    Returns:
        Indicador de validez y error si existe
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
            "error": str(e),
            "error_type": "parse_error"
        }
    except Exception as e:
        return {
            "valid": False,
            "error": str(e),
            "error_type": "unexpected_error"
        }


# ============================================================================
# ENDPOINTS DE RESOLUCIÓN
# ============================================================================

@router.post("/resolve", response_model=ResolveResponse)
async def resolve_query(request: ResolveRequest):
    """
    Resuelve una consulta Prolog y retorna el árbol SLD completo.
    
    Este endpoint construye el árbol completo de una vez.
    Para visualización paso a paso, usar WebSocket.
    
    Args:
        request: Programa, consulta, estrategia y configuración
    
    Returns:
        Árbol SLD completo con soluciones y estadísticas
    
    Raises:
        HTTPException: Si hay error de parsing o resolución
    """
    try:
        # 1. Parsear programa
        program = parse(request.program)
        
        if len(program.clauses) == 0:
            raise HTTPException(
                status_code=400,
                detail="Program must contain at least one clause"
            )
        
        # 2. Parsear consulta
        query = parse_query(request.query)
        
        if len(query.goals) == 0:
            raise HTTPException(
                status_code=400,
                detail="Query must contain at least one goal"
            )
        
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
            "max_depth_reached": max(node.depth for node in tree.all_nodes) if tree.all_nodes else 0,
            "success_nodes": sum(1 for n in tree.all_nodes if n.is_success()),
            "failure_nodes": sum(1 for n in tree.all_nodes if n.is_failure()),
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


# ============================================================================
# WEBSOCKET PARA RESOLUCIÓN PASO A PASO
# ============================================================================

@router.websocket("/ws/resolve-step")
async def resolve_step_websocket(websocket: WebSocket):
    """
    WebSocket endpoint para resolución paso a paso.
    
    Protocolo:
    
    Cliente envía:
    {
        "program": "...",
        "query": "...",
        "strategy": "leftmost",
        "max_depth": 20,
        "delay": 0.1
    }
    
    Servidor envía updates:
    {
        "type": "step",
        "step": 1,
        "tree": {...},
        "current_node": {...},
        "status": "expanded"
    }
    
    Al finalizar:
    {
        "type": "complete",
        "solutions": [...],
        "stats": {...}
    }
    
    En caso de error:
    {
        "type": "error",
        "error": "..."
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
        delay = data.get("delay", 0.1)  # Pausa entre pasos
        
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
            
            # Pausa para visualización
            await asyncio.sleep(delay)
        
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
        
    except ParseError as e:
        await websocket.send_json({
            "type": "error",
            "error": f"Parse error: {str(e)}",
            "error_type": "parse_error"
        })
    
    except Exception as e:
        await websocket.send_json({
            "type": "error",
            "error": str(e),
            "error_type": "unexpected_error"
        })
    
    finally:
        await websocket.close()