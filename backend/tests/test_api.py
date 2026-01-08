"""
test_api.py - Tests de la API REST

Tests esenciales:
- Health check
- Endpoints de parsing
- Endpoints de resolución
- Manejo de errores
"""

import pytest
from fastapi.testclient import TestClient
from src.api.main import app


# Cliente de prueba
client = TestClient(app)


class TestHealthCheck:
    """Tests del endpoint de health check"""
    
    def test_health_endpoint(self):
        """Health check debe retornar 200"""
        response = client.get("/api/health")
        assert response.status_code == 200
        
        data = response.json()
        assert data["status"] == "healthy"
        assert "service" in data
        assert "version" in data


class TestParseEndpoint:
    """Tests del endpoint /api/parse"""
    
    def test_parse_simple_fact(self):
        """Parsear hecho simple"""
        response = client.post(
            "/api/parse",
            json={"code": "padre(juan, maria)."}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert len(data["clauses"]) == 1
        assert data["num_clauses"] == 1
    
    def test_parse_multiple_clauses(self):
        """Parsear múltiples cláusulas"""
        code = """
            padre(juan, maria).
            madre(ana, maria).
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """
        response = client.post(
            "/api/parse",
            json={"code": code}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["num_clauses"] == 3
    
    def test_parse_syntax_error(self):
        """Error de sintaxis debe retornar success=false"""
        response = client.post(
            "/api/parse",
            json={"code": "padre(juan, maria"}  # Falta paréntesis
        )
        
        assert response.status_code == 200  # No es error HTTP
        data = response.json()
        assert data["success"] is False
        assert data["error"] is not None
    
    def test_parse_empty_code(self):
        """Código vacío"""
        response = client.post(
            "/api/parse",
            json={"code": ""}
        )
        
        # Debe fallar por validación (min_length=1)
        assert response.status_code == 422


class TestValidateEndpoint:
    """Tests del endpoint /api/validate"""
    
    def test_validate_correct_code(self):
        """Código correcto debe validar"""
        response = client.post(
            "/api/validate",
            json={"code": "padre(juan, maria)."}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["valid"] is True
        assert data["error"] is None
    
    def test_validate_incorrect_code(self):
        """Código incorrecto no debe validar"""
        response = client.post(
            "/api/validate",
            json={"code": "padre(juan,"}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["valid"] is False
        assert data["error"] is not None


class TestResolveEndpoint:
    """Tests del endpoint /api/resolve"""
    
    def test_resolve_simple_query(self):
        """Resolver consulta simple"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["tree"] is not None
        assert len(data["solutions"]) == 1
        assert data["stats"]["total_nodes"] > 0
    
    def test_resolve_with_rule(self):
        """Resolver consulta con regla"""
        program = """
            padre(juan, maria).
            padre(pedro, juan).
            abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
        """
        response = client.post(
            "/api/resolve",
            json={
                "program": program,
                "query": "?- abuelo(pedro, maria).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert len(data["solutions"]) >= 1
    
    def test_resolve_no_solutions(self):
        """Consulta sin soluciones"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(pedro, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert len(data["solutions"]) == 0
    
    def test_resolve_parse_error_in_program(self):
        """Error de sintaxis en programa"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan",  # Error de sintaxis
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 400  # Bad Request
        data = response.json()
        assert "Parse error" in data["detail"]
    
    def test_resolve_parse_error_in_query(self):
        """Error de sintaxis en consulta"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan",  # Error de sintaxis
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 400
    
    def test_resolve_invalid_strategy(self):
        """Estrategia inválida"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan, X).",
                "strategy": "invalid_strategy",  # Inválido
                "max_depth": 20
            }
        )
        
        # Debe fallar por validación Pydantic
        assert response.status_code == 422
    
    def test_resolve_max_depth_validation(self):
        """Validación de max_depth"""
        # max_depth fuera de rango (debe ser 1-100)
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 200  # Fuera de rango
            }
        )
        
        assert response.status_code == 422


class TestExamplesEndpoint:
    """Tests del endpoint /api/examples"""
    
    def test_get_examples(self):
        """Obtener ejemplos"""
        response = client.get("/api/examples")
        
        assert response.status_code == 200
        data = response.json()
        assert "examples" in data
        assert len(data["examples"]) > 0
        
        # Cada ejemplo debe tener estructura correcta
        for example in data["examples"]:
            assert "name" in example
            assert "program" in example
            assert "queries" in example


class TestResponseStructure:
    """Tests de estructura de respuestas"""
    
    def test_parse_response_structure(self):
        """Estructura de ParseResponse"""
        response = client.post(
            "/api/parse",
            json={"code": "padre(juan, maria)."}
        )
        
        data = response.json()
        assert "success" in data
        assert "clauses" in data
        assert "num_clauses" in data
        assert "error" in data
    
    def test_resolve_response_structure(self):
        """Estructura de ResolveResponse"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        data = response.json()
        assert "success" in data
        assert "tree" in data
        assert "solutions" in data
        assert "stats" in data
        assert "error" in data
        
        # Estructura de stats
        stats = data["stats"]
        assert "total_nodes" in stats
        assert "solutions_found" in stats
        assert "max_depth_reached" in stats
        assert "success_nodes" in stats
        assert "failure_nodes" in stats
        
        # Estructura de solutions
        if data["solutions"]:
            solution = data["solutions"][0]
            assert "node_id" in solution
            assert "substitution" in solution
            assert "goals" in solution


class TestCORS:
    """Tests de CORS middleware"""
    
    def test_cors_headers_present(self):
        """Headers CORS deben estar presentes"""
        response = client.get(
                    "/api/health",
                    headers={"Origin": "0.0.0.0"}
                )        
        # Debe haber headers de CORS
        assert "access-control-allow-origin" in response.headers


class TestEdgeCases:
    """Tests de casos edge"""
    
    def test_empty_program_resolve(self):
        """Resolver con programa vacío"""
        response = client.post(
            "/api/resolve",
            json={
                "program": "",  # Vacío
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        # Debe fallar por validación (min_length)
        assert response.status_code == 422
    
    def test_very_complex_query(self):
        """Consulta muy compleja"""
        program = """
            a(1). a(2). a(3).
            b(1). b(2). b(3).
            c(1). c(2). c(3).
        """
        query = "?- a(X), b(Y), c(Z)."
        
        response = client.post(
            "/api/resolve",
            json={
                "program": program,
                "query": query,
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        # Debe haber 3*3*3 = 27 soluciones
        assert len(data["solutions"]) == 27


class TestPerformance:
    """Tests de rendimiento básicos"""
    
    def test_large_program_parses(self):
        """Programa grande debe parsear"""
        clauses = [f"fact{i}(a{i})." for i in range(100)]
        code = "\n".join(clauses)
        
        response = client.post(
            "/api/parse",
            json={"code": code}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["num_clauses"] == 100
    
    def test_reasonable_response_time(self):
        """Respuesta debe ser razonablemente rápida"""
        import time
        
        start = time.time()
        response = client.post(
            "/api/resolve",
            json={
                "program": "padre(juan, maria).",
                "query": "?- padre(juan, X).",
                "strategy": "leftmost",
                "max_depth": 20
            }
        )
        elapsed = time.time() - start
        
        assert response.status_code == 200
        # Debe responder en menos de 1 segundo
        assert elapsed < 1.0


# ============================================================================
# RUN TESTS
# ============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])