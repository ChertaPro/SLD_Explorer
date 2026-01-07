"""
resolution.py - Motor de Resolución SLD

Implementa el motor de resolución SLD (Selective Linear Definite clause resolution)
que es la base de la ejecución de programas Prolog.

SLD Resolution:
1. Seleccionar un goal del conjunto de goals
2. Buscar una cláusula que unifique con ese goal
3. Si unifica, reemplazar el goal por el cuerpo de la cláusula
4. Repetir hasta que no haya goals (éxito) o no haya cláusulas (fallo)

El resultado es un árbol de búsqueda donde:
- Nodos = estados de resolución (goals pendientes + sustitución)
- Aristas = aplicación de una cláusula
- Hojas = éxito (sin goals) o fallo (sin cláusulas aplicables)
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple, Set
from enum import Enum
import uuid

from ..parser.terms import Term, Variable, Compound, Substitution  
from ..parser import Clause, Program  
from ..unification.unify import unify, rename_variables, UnificationError  


# ============================================================================
# ESTRUCTURAS DE DATOS
# ============================================================================

class NodeStatus(Enum):
    """Estado de un nodo en el árbol SLD"""
    PENDING = "pending"      # Aún no explorado
    SUCCESS = "success"      # Rama exitosa (sin goals)
    FAILURE = "failure"      # No hay cláusulas aplicables
    EXPANDED = "expanded"    # Ya expandido (nodo interno)


@dataclass
class SLDNode:
    """
    Representa un nodo en el árbol de resolución SLD.
    
    Cada nodo contiene:
    - goals: Lista de goals pendientes por resolver
    - substitution: Sustitución acumulada hasta este punto
    - parent: Nodo padre (None para la raíz)
    - children: Lista de nodos hijos
    - clause_used: Cláusula usada para llegar a este nodo
    - status: Estado del nodo
    - depth: Profundidad en el árbol
    """
    goals: List[Compound]
    substitution: Substitution
    node_id: str = field(default_factory=lambda: str(uuid.uuid4())[:8])
    parent: Optional['SLDNode'] = None
    children: List['SLDNode'] = field(default_factory=list)
    clause_used: Optional[Clause] = None
    selected_goal_index: int = 0
    status: NodeStatus = NodeStatus.PENDING
    depth: int = 0
    
    def is_success(self) -> bool:
        """Un nodo es exitoso si no tiene goals pendientes"""
        return len(self.goals) == 0
    
    def is_failure(self) -> bool:
        """Un nodo es fallido si tiene goals pero no puede expandirse"""
        return len(self.goals) > 0 and self.status == NodeStatus.FAILURE
    
    def selected_goal(self) -> Optional[Compound]:
        """Retorna el goal seleccionado para resolución"""
        if 0 <= self.selected_goal_index < len(self.goals):
            return self.goals[self.selected_goal_index]
        return None
    
    def __str__(self) -> str:
        if len(self.goals) == 0:
            return "□ (success)"
        goals_str = ", ".join(str(g) for g in self.goals)
        subst_str = str(self.substitution) if self.substitution else "{}"
        return f"Goals: [{goals_str}] | θ: {subst_str}"
    
    def to_dict(self) -> dict:
        """Serializa el nodo a diccionario para JSON"""
        return {
            "id": self.node_id,
            "goals": [str(g) for g in self.goals],
            "substitution": str(self.substitution),
            "status": self.status.value,
            "depth": self.depth,
            "selected_goal_index": self.selected_goal_index,
            "selected_goal": str(self.selected_goal()) if self.selected_goal() else None,
            "clause_used": str(self.clause_used) if self.clause_used else None,
            "children": [child.node_id for child in self.children],
            "parent": self.parent.node_id if self.parent else None
        }


@dataclass
class SLDTree:
    """
    Representa el árbol de resolución SLD completo.
    
    Attributes:
        root: Nodo raíz del árbol
        program: Programa Prolog usado
        all_nodes: Lista de todos los nodos (para fácil acceso)
        max_depth: Profundidad máxima permitida (prevenir loops infinitos)
    """
    root: SLDNode
    program: Program
    all_nodes: List[SLDNode] = field(default_factory=list)
    max_depth: int = 20
    
    def __post_init__(self):
        self.all_nodes.append(self.root)
    
    def find_solutions(self) -> List[Tuple[SLDNode, Substitution]]:
        """
        Encuentra todas las soluciones (nodos exitosos) en el árbol.
        
        Returns:
            Lista de tuplas (nodo, sustitución) para cada solución
        """
        solutions = []
        for node in self.all_nodes:
            if node.is_success():
                solutions.append((node, node.substitution))
        return solutions
    
    def to_dict(self) -> dict:
        """Serializa el árbol completo a diccionario"""
        return {
            "root": self.root.node_id,
            "nodes": [node.to_dict() for node in self.all_nodes],
            "program": str(self.program),
            "solutions": [
                {
                    "node_id": node.node_id,
                    "substitution": str(subst)
                }
                for node, subst in self.find_solutions()
            ]
        }


# ============================================================================
# MOTOR DE RESOLUCIÓN SLD
# ============================================================================

class SLDResolver:
    """
    Motor de resolución SLD que construye el árbol de búsqueda.
    
    Estrategias de selección de goals:
    - leftmost: Siempre selecciona el primer goal (estrategia de Prolog)
    - rightmost: Selecciona el último goal
    """
    
    def __init__(self, program: Program, max_depth: int = 20):
        """
        Inicializa el resolver.
        
        Args:
            program: Programa Prolog a usar
            max_depth: Profundidad máxima del árbol (prevenir loops)
        """
        self.program = program
        self.max_depth = max_depth
        self.clause_counter = 0  # Para renombrar variables
    
    def resolve(self, query_goals: List[Compound], 
                strategy: str = "leftmost") -> SLDTree:
        """
        Ejecuta la resolución SLD para una consulta.
        
        Args:
            query_goals: Lista de goals de la consulta
            strategy: Estrategia de selección ("leftmost" o "rightmost")
        
        Returns:
            Árbol SLD completo
        
        Algoritmo:
            1. Crear nodo raíz con los goals de la consulta
            2. Expandir nodo actual:
                a. Seleccionar un goal según estrategia
                b. Buscar cláusulas que unifiquen con el goal
                c. Para cada cláusula unificable, crear hijo
            3. Repetir para cada nodo pending hasta max_depth
        """
        # Crear nodo raíz
        root = SLDNode(
            goals=query_goals,
            substitution=Substitution(),
            depth=0,
            status=NodeStatus.PENDING
        )
        
        # Crear árbol
        tree = SLDTree(root=root, program=self.program, max_depth=self.max_depth)
        
        # Cola de nodos por expandir (BFS)
        queue = [root]
        
        while queue:
            node = queue.pop(0)
            
            # Verificar profundidad
            if node.depth >= self.max_depth:
                node.status = NodeStatus.FAILURE
                continue
            
            # Si no hay goals, es éxito
            if node.is_success():
                node.status = NodeStatus.SUCCESS
                continue
            
            # Expandir nodo
            children = self._expand_node(node, strategy)
            
            if len(children) == 0:
                # No hay cláusulas aplicables = fallo
                node.status = NodeStatus.FAILURE
            else:
                # Nodo expandido exitosamente
                node.status = NodeStatus.EXPANDED
                node.children = children
                tree.all_nodes.extend(children)
                queue.extend(children)
        
        return tree
    
    def _expand_node(self, node: SLDNode, strategy: str) -> List[SLDNode]:
        """
        Expande un nodo aplicando todas las cláusulas posibles.
        
        Args:
            node: Nodo a expandir
            strategy: Estrategia de selección de goal
        
        Returns:
            Lista de nodos hijos creados
        """
        # Seleccionar goal según estrategia
        if strategy == "leftmost":
            goal_index = 0
        elif strategy == "rightmost":
            goal_index = len(node.goals) - 1
        else:
            goal_index = 0  # Default leftmost
        
        if goal_index >= len(node.goals):
            return []
        
        selected_goal = node.goals[goal_index]
        node.selected_goal_index = goal_index
        
        # Buscar cláusulas aplicables
        children = []
        
        for clause in self.program.clauses:
            # Renombrar variables de la cláusula para evitar colisiones
            self.clause_counter += 1
            renamed_clause = self._rename_clause(clause, f"_{self.clause_counter}")
            
            # Intentar unificar
            try:
                # Unificar goal con head de la cláusula
                new_subst = unify(
                    selected_goal, 
                    renamed_clause.head, 
                    node.substitution
                )
                
                # Construir nuevos goals
                new_goals = []
                
                # Agregar goals antes del seleccionado
                new_goals.extend(node.goals[:goal_index])
                
                # Si la cláusula tiene body, agregar sus goals
                if renamed_clause.body:
                    new_goals.extend(renamed_clause.body)
                
                # Agregar goals después del seleccionado
                new_goals.extend(node.goals[goal_index + 1:])
                
                # Aplicar sustitución a todos los goals
                new_goals = [
                    g.apply_substitution(new_subst) 
                    for g in new_goals
                ]
                
                # Crear nodo hijo
                child = SLDNode(
                    goals=new_goals,
                    substitution=new_subst,
                    parent=node,
                    clause_used=renamed_clause,
                    depth=node.depth + 1,
                    status=NodeStatus.PENDING
                )
                
                children.append(child)
                
            except UnificationError:
                # Esta cláusula no unifica, continuar
                continue
        
        return children
    
    def _rename_clause(self, clause: Clause, suffix: str) -> Clause:
        """
        Renombra todas las variables en una cláusula.
        
        Args:
            clause: Cláusula original
            suffix: Sufijo a agregar a las variables
        
        Returns:
            Nueva cláusula con variables renombradas
        """
        new_head = rename_variables(clause.head, suffix)
        new_body = None
        
        if clause.body:
            new_body = [
                rename_variables(goal, suffix) 
                for goal in clause.body
            ]
        
        return Clause(head=new_head, body=new_body)
    
    def resolve_step_by_step(self, query_goals: List[Compound],
                             strategy: str = "leftmost"):
        """
        Generador que produce el árbol paso a paso.
        
        Útil para animaciones y visualización incremental.
        
        Yields:
            (tree, current_node) en cada paso
        """
        root = SLDNode(
            goals=query_goals,
            substitution=Substitution(),
            depth=0,
            status=NodeStatus.PENDING
        )
        
        tree = SLDTree(root=root, program=self.program, max_depth=self.max_depth)
        queue = [root]
        
        yield tree, root  # Estado inicial
        
        while queue:
            node = queue.pop(0)
            
            if node.depth >= self.max_depth:
                node.status = NodeStatus.FAILURE
                yield tree, node
                continue
            
            if node.is_success():
                node.status = NodeStatus.SUCCESS
                yield tree, node
                continue
            
            children = self._expand_node(node, strategy)
            
            if len(children) == 0:
                node.status = NodeStatus.FAILURE
            else:
                node.status = NodeStatus.EXPANDED
                node.children = children
                tree.all_nodes.extend(children)
                queue.extend(children)
            
            yield tree, node  # Después de expandir


# ============================================================================
# TESTS
# ============================================================================

def test_sld_resolution():
    """Tests del motor SLD"""
    from ..parser import parse, parse_query  
    
    print("=" * 70)
    print("TESTS DE RESOLUCIÓN SLD")
    print("=" * 70)
    
    # Programa de ejemplo
    code = """
        padre(juan, maria).
        padre(juan, pedro).
        padre(pedro, ana).
        
        abuelo(X, Z) :- padre(X, Y), padre(Y, Z).
    """
    
    program = parse(code)
    print("\nPrograma:")
    print(program)
    
    # Test 1: Consulta simple
    print("\n" + "=" * 70)
    print("Test 1: Consulta simple - ?- padre(juan, X).")
    print("=" * 70)
    
    query = parse_query("?- padre(juan, X).")
    resolver = SLDResolver(program)
    tree = resolver.resolve(query.goals)
    
    print(f"\nNodos generados: {len(tree.all_nodes)}")
    print("\nÁrbol de resolución:")
    print_tree(tree.root, indent=0)
    
    solutions = tree.find_solutions()
    print(f"\nSoluciones encontradas: {len(solutions)}")
    for i, (node, subst) in enumerate(solutions, 1):
        print(f"  {i}. {subst}")
    
    # Test 2: Consulta con regla
    print("\n" + "=" * 70)
    print("Test 2: Consulta con regla - ?- abuelo(juan, Z).")
    print("=" * 70)
    
    query = parse_query("?- abuelo(juan, Z).")
    tree = resolver.resolve(query.goals)
    
    print(f"\nNodos generados: {len(tree.all_nodes)}")
    print("\nÁrbol de resolución:")
    print_tree(tree.root, indent=0)
    
    solutions = tree.find_solutions()
    print(f"\nSoluciones encontradas: {len(solutions)}")
    for i, (node, subst) in enumerate(solutions, 1):
        print(f"  {i}. {subst}")


def print_tree(node: SLDNode, indent: int = 0):
    """Imprime el árbol de forma jerárquica"""
    prefix = "  " * indent
    status_symbol = {
        NodeStatus.SUCCESS: "✓",
        NodeStatus.FAILURE: "✗",
        NodeStatus.EXPANDED: "→",
        NodeStatus.PENDING: "?"
    }
    symbol = status_symbol.get(node.status, "?")
    
    print(f"{prefix}{symbol} {node}")
    if node.clause_used:
        print(f"{prefix}  ← {node.clause_used}")
    
    for child in node.children:
        print_tree(child, indent + 1)


if __name__ == "__main__":
    test_sld_resolution()