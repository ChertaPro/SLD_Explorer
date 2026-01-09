"""
resolution.py - Motor de Resolución SLD (Versión Exhaustiva)

Modificado para explorar TODAS las posibilidades y generar árbol completo
con renombramiento de variables por profundidad/rama.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple
from enum import Enum

from ..parser.terms import Term, Variable, Compound, Substitution  
from ..parser import Clause, Program  
from ..unification.unify import unify, rename_variables, UnificationError  


# ============================================================================
# ESTRUCTURAS DE DATOS
# ============================================================================

class NodeStatus(Enum):
    """Estado de un nodo en el árbol SLD"""
    PENDING = "pending"
    SUCCESS = "success"
    FAILURE = "failure"
    EXPANDED = "expanded"


@dataclass
class SLDNode:
    """Nodo del árbol SLD con información completa"""
    goals: List[Compound]
    substitution: Substitution
    node_id: str = field(default_factory=lambda: _generate_node_id())
    parent: Optional['SLDNode'] = None
    children: List['SLDNode'] = field(default_factory=list)
    clause_used: Optional[Clause] = None
    selected_goal_index: int = 0
    status: NodeStatus = NodeStatus.PENDING
    depth: int = 0
    branch_number: int = 0  # Número de rama en ese nivel
    umg: str = ""  # Unificador Más General
    src: str = ""  # Substitución Resultado Computado (solo en success)
    
    def is_success(self) -> bool:
        return len(self.goals) == 0
    
    def is_failure(self) -> bool:
        return len(self.goals) > 0 and self.status == NodeStatus.FAILURE
    
    def selected_goal(self) -> Optional[Compound]:
        if 0 <= self.selected_goal_index < len(self.goals):
            return self.goals[self.selected_goal_index]
        return None
    
    def __str__(self) -> str:
        if len(self.goals) == 0:
            return f"□ (success) | SRC = {self.src}"
        goals_str = ", ".join(str(g) for g in self.goals)
        return f"Goals: [{goals_str}] | UMG{self.depth} = {self.umg}"
    
    def to_dict(self) -> dict:
        return {
            "id": self.node_id,
            "goals": [str(g) for g in self.goals],
            "substitution": str(self.substitution),
            "status": self.status.value,
            "depth": self.depth,
            "branch_number": self.branch_number,
            "selected_goal_index": self.selected_goal_index,
            "selected_goal": str(self.selected_goal()) if self.selected_goal() else None,
            "clause_used": str(self.clause_used) if self.clause_used else None,
            "umg": self.umg,
            "src": self.src if self.is_success() else "",
            "children": [child.node_id for child in self.children],
            "parent": self.parent.node_id if self.parent else None
        }


# Contador global para IDs de nodos
_node_counter = 0

def _generate_node_id():
    global _node_counter
    _node_counter += 1
    return f"node_{_node_counter}"


@dataclass
class SLDTree:
    """Árbol SLD completo"""
    root: SLDNode
    program: Program
    all_nodes: List[SLDNode] = field(default_factory=list)
    max_depth: int = 20
    
    def __post_init__(self):
        self.all_nodes.append(self.root)
    
    def find_solutions(self) -> List[Tuple[SLDNode, Substitution]]:
        solutions = []
        for node in self.all_nodes:
            if node.is_success():
                solutions.append((node, node.substitution))
        return solutions
    
    def to_dict(self) -> dict:
        return {
            "root": self.root.node_id,
            "nodes": [node.to_dict() for node in self.all_nodes],
            "program": str(self.program),
            "solutions": [
                {
                    "node_id": node.node_id,
                    "substitution": str(subst),
                    "src": node.src
                }
                for node, subst in self.find_solutions()
            ]
        }


# ============================================================================
# MOTOR SLD EXHAUSTIVO
# ============================================================================

class SLDResolver:
    """Motor SLD que explora TODAS las posibilidades"""
    
    def __init__(self, program: Program, max_depth: int = 20):
        self.program = program
        self.max_depth = max_depth
        self.clause_usage_counter = {}  # Contador por (depth, branch)
    
    def resolve(self, query_goals: List[Compound], strategy: str = "leftmost") -> SLDTree:
        """Resolución SLD exhaustiva"""
        global _node_counter
        _node_counter = 0
        self.clause_usage_counter = {}
        
        # Nodo raíz
        root = SLDNode(
            goals=query_goals,
            substitution=Substitution(),
            depth=0,
            branch_number=0,
            status=NodeStatus.PENDING
        )
        
        tree = SLDTree(root=root, program=self.program, max_depth=self.max_depth)
        
        # BFS para explorar todo el árbol
        queue = [root]
        
        while queue:
            node = queue.pop(0)
            
            # Límite de profundidad
            if node.depth >= self.max_depth:
                node.status = NodeStatus.FAILURE
                continue
            
            # Éxito
            if node.is_success():
                node.status = NodeStatus.SUCCESS
                # Calcular SRC (variables originales de la consulta)
                node.src = self._calculate_src(node.substitution, query_goals)
                continue
            
            # Expandir nodo con TODAS las cláusulas aplicables
            children = self._expand_node_exhaustive(node, strategy)
            
            if len(children) == 0:
                node.status = NodeStatus.FAILURE
            else:
                node.status = NodeStatus.EXPANDED
                node.children = children
                tree.all_nodes.extend(children)
                queue.extend(children)
        
        return tree
    
    def _expand_node_exhaustive(self, node: SLDNode, strategy: str) -> List[SLDNode]:
        """Expande un nodo probando TODAS las cláusulas del programa"""
        # Seleccionar goal
        if strategy == "leftmost":
            goal_index = 0
        elif strategy == "rightmost":
            goal_index = len(node.goals) - 1
        else:
            goal_index = 0
        
        if goal_index >= len(node.goals):
            return []
        
        selected_goal = node.goals[goal_index]
        node.selected_goal_index = goal_index
        
        children = []
        branch_number = 1  # Contador de ramas en este nivel
        
        # Probar CADA cláusula del programa
        for clause in self.program.clauses:
            # Renombrar variables con formato: Var_profundidad_rama
            suffix = f"_{node.depth + 1}{branch_number}"
            renamed_clause = self._rename_clause(clause, suffix)
            
            try:
                # Intentar unificar
                new_subst = unify(selected_goal, renamed_clause.head, node.substitution)
                
                # Construir nuevos goals
                new_goals = []
                new_goals.extend(node.goals[:goal_index])
                
                if renamed_clause.body:
                    new_goals.extend(renamed_clause.body)
                
                new_goals.extend(node.goals[goal_index + 1:])
                
                # Aplicar sustitución
                new_goals = [g.apply_substitution(new_subst) for g in new_goals]
                
                # Calcular UMG para este paso
                umg = self._calculate_umg(node.substitution, new_subst)
                
                # Crear nodo hijo
                child = SLDNode(
                    goals=new_goals,
                    substitution=new_subst,
                    parent=node,
                    clause_used=renamed_clause,
                    depth=node.depth + 1,
                    branch_number=branch_number,
                    status=NodeStatus.PENDING,
                    umg=umg
                )
                
                children.append(child)
                branch_number += 1
                
            except UnificationError:
                # Esta cláusula no unifica, continuar con la siguiente
                continue
        
        return children
    
    def _rename_clause(self, clause: Clause, suffix: str) -> Clause:
        """Renombra variables de una cláusula"""
        new_head = rename_variables(clause.head, suffix)
        new_body = None
        
        if clause.body:
            new_body = [rename_variables(goal, suffix) for goal in clause.body]
        
        return Clause(head=new_head, body=new_body)
    
    def _calculate_umg(self, old_subst: Substitution, new_subst: Substitution) -> str:
        """Calcula el UMG (Unificador Más General) para mostrar"""
        # Mostrar solo las nuevas bindings
        new_bindings = {}
        for var, term in new_subst.items():
            if var not in old_subst or old_subst[var] != term:
                new_bindings[var] = term
        
        if not new_bindings:
            return "{}"
        
        items = [f"{var.name} / {term}" for var, term in new_bindings.items()]
        return "{ " + ", ".join(items) + " }"
    
    def _calculate_src(self, subst: Substitution, original_goals: List[Compound]) -> str:
        """Calcula el SRC (Substitución Resultado Computado)"""
        # Extraer solo las variables de la consulta original
        original_vars = set()
        for goal in original_goals:
            original_vars.update(goal.get_variables())
        
        relevant_bindings = {}
        for var in original_vars:
            if var in subst:
                relevant_bindings[var] = subst[var]
        
        if not relevant_bindings:
            return "{}"
        
        items = [f"{var.name} / {term}" for var, term in relevant_bindings.items()]
        return "{ " + ", ".join(items) + " }"
    
    def resolve_step_by_step(self, query_goals: List[Compound], strategy: str = "leftmost"):
        """Generador para resolución paso a paso"""
        global _node_counter
        _node_counter = 0
        
        root = SLDNode(
            goals=query_goals,
            substitution=Substitution(),
            depth=0,
            branch_number=0,
            status=NodeStatus.PENDING
        )
        
        tree = SLDTree(root=root, program=self.program, max_depth=self.max_depth)
        queue = [root]
        
        yield tree, root
        
        while queue:
            node = queue.pop(0)
            
            if node.depth >= self.max_depth:
                node.status = NodeStatus.FAILURE
                yield tree, node
                continue
            
            if node.is_success():
                node.status = NodeStatus.SUCCESS
                node.src = self._calculate_src(node.substitution, query_goals)
                yield tree, node
                continue
            
            children = self._expand_node_exhaustive(node, strategy)
            
            if len(children) == 0:
                node.status = NodeStatus.FAILURE
            else:
                node.status = NodeStatus.EXPANDED
                node.children = children
                tree.all_nodes.extend(children)
                queue.extend(children)
            
            yield tree, node


# ============================================================================
# TESTS
# ============================================================================

def test_sld_resolution():
    """Test con el ejemplo de la foto"""
    from ..parser import parse, parse_query
    
    print("=" * 70)
    print("TEST DE RESOLUCIÓN SLD EXHAUSTIVA")
    print("=" * 70)
    
    # Programa del ejemplo
    code = """
        padre(luis, alicia).
        padre(luis, jose).
        madre(alicia, dario).
        madre(jose, ana).
        
        abuelo(X, Y) :- padre(X, Z), madre(Z, Y).
    """
    
    program = parse(code)
    print("\nPrograma:")
    print(program)
    
    print("\n" + "=" * 70)
    print("Consulta: ?- abuelo(luis, X).")
    print("=" * 70)
    
    query = parse_query("?- abuelo(luis, X).")
    resolver = SLDResolver(program, max_depth=10)
    tree = resolver.resolve(query.goals)
    
    print(f"\nNodos generados: {len(tree.all_nodes)}")
    print("\nÁrbol de resolución:")
    print_tree(tree.root, indent=0)
    
    solutions = tree.find_solutions()
    print(f"\nSoluciones encontradas: {len(solutions)}")
    for i, (node, subst) in enumerate(solutions, 1):
        print(f"  {i}. SRC = {node.src}")


def print_tree(node: SLDNode, indent: int = 0):
    """Imprime el árbol jerárquicamente"""
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