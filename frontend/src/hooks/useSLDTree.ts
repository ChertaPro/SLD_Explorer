/**
 * hooks/useSLDTree.ts - Hook personalizado para manejo del árbol SLD
 */

import { useState, useCallback, useMemo } from 'react';
import { Node, Edge, Position } from 'reactflow';
import type { SLDTreeData, SLDNodeData, CustomNodeData } from '../types/types';

// ============================================================================
// CONFIGURACIÓN DE LAYOUT
// ============================================================================

const LAYOUT_CONFIG = {
  horizontalSpacing: 350,
  verticalSpacing: 200,
  nodeWidth: 250,
  nodeHeight: 150,
};

// ============================================================================
// TIPOS
// ============================================================================

interface UseTreeResult {
  nodes: Node<CustomNodeData>[];
  edges: Edge[];
  layoutTree: (treeData: SLDTreeData) => void;
  clearTree: () => void;
  animateNode: (nodeId: string) => void;
}

// ============================================================================
// HOOK
// ============================================================================

export const useSLDTree = (): UseTreeResult => {
  const [nodes, setNodes] = useState<Node<CustomNodeData>[]>([]);
  const [edges, setEdges] = useState<Edge[]>([]);

  /**
   * Calcula el color del nodo según su estado
   */
  const getNodeColor = useCallback((status: string) => {
    switch (status) {
      case 'success':
        return {
          bg: 'bg-gradient-to-br from-green-50 to-emerald-100',
          border: 'border-green-500',
          glow: 'shadow-green-500/50',
        };
      case 'failure':
        return {
          bg: 'bg-gradient-to-br from-red-50 to-rose-100',
          border: 'border-red-500',
          glow: 'shadow-red-500/50',
        };
      case 'expanded':
        return {
          bg: 'bg-gradient-to-br from-blue-50 to-cyan-100',
          border: 'border-blue-500',
          glow: 'shadow-blue-500/50',
        };
      default:
        return {
          bg: 'bg-gradient-to-br from-gray-50 to-slate-100',
          border: 'border-gray-400',
          glow: 'shadow-gray-500/50',
        };
    }
  }, []);

  /**
   * Calcula el estilo de la arista según el estado del nodo hijo
   */
  const getEdgeStyle = useCallback((status: string) => {
    switch (status) {
      case 'success':
        return {
          stroke: '#10b981',
          strokeWidth: 3,
        };
      case 'failure':
        return {
          stroke: '#ef4444',
          strokeWidth: 3,
        };
      case 'expanded':
        return {
          stroke: '#3b82f6',
          strokeWidth: 2,
        };
      default:
        return {
          stroke: '#9ca3af',
          strokeWidth: 2,
        };
    }
  }, []);

  /**
   * Organiza el árbol usando un layout jerárquico
   */
  const layoutTree = useCallback(
    (treeData: SLDTreeData) => {
      if (!treeData || !treeData.nodes || treeData.nodes.length === 0) {
        setNodes([]);
        setEdges([]);
        return;
      }

      // Agrupar nodos por profundidad
      const nodesByDepth = new Map<number, SLDNodeData[]>();
      treeData.nodes.forEach((node) => {
        if (!nodesByDepth.has(node.depth)) {
          nodesByDepth.set(node.depth, []);
        }
        nodesByDepth.get(node.depth)!.push(node);
      });

      const flowNodes: Node<CustomNodeData>[] = [];
      const flowEdges: Edge[] = [];

      // Posicionar nodos por nivel
      nodesByDepth.forEach((nodesAtDepth, depth) => {
        const totalWidth = (nodesAtDepth.length - 1) * LAYOUT_CONFIG.horizontalSpacing;
        const startX = -totalWidth / 2;

        nodesAtDepth.forEach((node, idx) => {
          const colors = getNodeColor(node.status);

          flowNodes.push({
            id: node.id,
            type: 'custom',
            position: {
              x: startX + idx * LAYOUT_CONFIG.horizontalSpacing,
              y: depth * LAYOUT_CONFIG.verticalSpacing,
            },
            data: {
              id: node.id,
              goals: node.goals,
              substitution: node.substitution,
              status: node.status,
              depth: node.depth,
              selected_goal_index: node.selected_goal_index,
              clause_used: node.clause_used,
              ...colors,
            },
            sourcePosition: Position.Bottom,
            targetPosition: Position.Top,
          });

          // Crear arista al padre
          if (node.parent) {
            const edgeStyle = getEdgeStyle(node.status);

            flowEdges.push({
              id: `${node.parent}-${node.id}`,
              source: node.parent,
              target: node.id,
              type: 'smoothstep',
              animated: node.status === 'pending',
              style: edgeStyle,
              markerEnd: {
                type: 'arrowclosed' as const,
                color: edgeStyle.stroke,
              },
            });
          }
        });
      });

      setNodes(flowNodes);
      setEdges(flowEdges);
    },
    [getNodeColor, getEdgeStyle]
  );

  /**
   * Limpia el árbol
   */
  const clearTree = useCallback(() => {
    setNodes([]);
    setEdges([]);
  }, []);

  /**
   * Anima un nodo específico
   */
  const animateNode = useCallback((nodeId: string) => {
    setNodes((prevNodes) =>
      prevNodes.map((node) =>
        node.id === nodeId
          ? {
              ...node,
              data: {
                ...node.data,
                isAnimating: true,
              },
            }
          : node
      )
    );

    // Remover animación después de 1 segundo
    setTimeout(() => {
      setNodes((prevNodes) =>
        prevNodes.map((node) =>
          node.id === nodeId
            ? {
                ...node,
                data: {
                  ...node.data,
                  isAnimating: false,
                },
              }
            : node
        )
      );
    }, 1000);
  }, []);

  return {
    nodes,
    edges,
    layoutTree,
    clearTree,
    animateNode,
  };
};

// ============================================================================
// HOOK PARA EJEMPLOS
// ============================================================================

export const useExamples = () => {
  const [examples, setExamples] = useState<any[]>([]);
  const [loading, setLoading] = useState(false);

  const loadExamples = useCallback(async () => {
    setLoading(true);
    try {
      const { api } = await import('../services/api');
      const response = await api.getExamples();
      setExamples(response.examples);
    } catch (error) {
      console.error('Error loading examples:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  return { examples, loading, loadExamples };
};