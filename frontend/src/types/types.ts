/**
 * types/types.ts - Type definitions para SLD-Explorer
 * Versión mejorada con UMG y SRC
 */

// ============================================================================
// TIPOS DE LA API
// ============================================================================

export interface ParseRequest {
  code: string;
}

export interface ParseResponse {
  success: boolean;
  clauses: string[] | null;
  num_clauses: number;
  error: string | null;
}

export interface ResolveRequest {
  program: string;
  query: string;
  strategy: 'leftmost' | 'rightmost';
  max_depth: number;
}

export interface Solution {
  node_id: string;
  substitution: string;
  goals: string[];
}

export interface Stats {
  total_nodes: number;
  solutions_found: number;
  max_depth_reached: number;
  success_nodes: number;
  failure_nodes: number;
}

export interface SLDNodeData {
  id: string;
  goals: string[];
  substitution: string;
  status: 'pending' | 'success' | 'failure' | 'expanded';
  depth: number;
  branch_number: number;
  selected_goal_index: number;
  selected_goal: string | null;
  clause_used: string | null;
  umg: string;  // Unificador Más General
  src: string;  // Substitución Resultado Computado (solo en success)
  children: string[];
  parent: string | null;
}

export interface SLDTreeData {
  root: string;
  nodes: SLDNodeData[];
  program: string;
  solutions: Array<{
    node_id: string;
    substitution: string;
    src: string;
  }>;
}

export interface ResolveResponse {
  success: boolean;
  tree: SLDTreeData | null;
  solutions: Solution[] | null;
  stats: Stats | null;
  error: string | null;
}

export interface Example {
  name: string;
  description?: string;
  program: string;
  queries: string[];
}

export interface ExamplesResponse {
  examples: Example[];
}

// ============================================================================
// TIPOS DE REACT FLOW
// ============================================================================

export interface CustomNodeData {
  id: string;
  goals: string[];
  substitution: string;
  status: 'pending' | 'success' | 'failure' | 'expanded';
  depth: number;
  branch_number: number;
  selected_goal_index: number;
  clause_used: string | null;
  umg: string;  // Unificador Más General
  src: string;  // Substitución Resultado Computado
  isAnimating?: boolean;
}

export type NodeType = 'custom';

// ============================================================================
// TIPOS DE ESTADO DE LA APP
// ============================================================================

export interface AppState {
  program: string;
  query: string;
  strategy: 'leftmost' | 'rightmost';
  maxDepth: number;
  isResolving: boolean;
  error: string | null;
  solutions: Solution[];
  stats: Stats | null;
  currentExample: string | null;
}

// ============================================================================
// TIPOS DE ANIMACIÓN
// ============================================================================

export interface AnimationConfig {
  duration: number;
  delay: number;
  type: 'spring' | 'tween';
}

export interface TreeAnimationState {
  currentStep: number;
  maxSteps: number;
  isPlaying: boolean;
  speed: number;
}