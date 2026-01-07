"""
main.py - Punto de entrada de la API FastAPI

Configuración y lanzamiento de la aplicación.
"""

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from .routes import router


# ============================================================================
# CONFIGURACIÓN DE LA APP
# ============================================================================

def create_app() -> FastAPI:
    """
    Factory para crear la aplicación FastAPI.
    
    Returns:
        Aplicación configurada
    """
    app = FastAPI(
        title="SLD-Explorer API",
        description="API para visualización de árboles SLD de Prolog",
        version="1.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
    )
    
    # CORS para permitir requests desde el frontend
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],  # En producción, especificar dominios exactos
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    
    # Incluir rutas
    app.include_router(router)
    
    return app


# Crear instancia de la app
app = create_app()


# ============================================================================
# MAIN
# ============================================================================

if __name__ == "__main__":
    import uvicorn
    
    uvicorn.run(
        "src.api.main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )