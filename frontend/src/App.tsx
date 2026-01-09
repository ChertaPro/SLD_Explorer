import React, { useState, useCallback, useEffect } from 'react';
import ReactFlow, {
  Controls,
  Background,
  MiniMap,
  Panel,
  useNodesState,
  useEdgesState,
} from 'reactflow';
import 'reactflow/dist/style.css';
import { motion, AnimatePresence } from 'framer-motion';
import { 
  Sparkles, 
  BookOpen, 
  Lightbulb, 
  AlertCircle,
  X,
  ChevronLeft,
  ChevronRight
} from 'lucide-react';

import { CustomNode } from './components/CustomNode';
import { CodeEditor } from './components/CodeEditor';
import { ControlPanel } from './components/ControlPanel';
import { useSLDTree, useExamples } from './hooks/useSLDTree';
import { api, handleAPIError } from './services/api';
import type { Solution } from './types/types';

const nodeTypes = {
  custom: CustomNode,
};

const EXAMPLES = [
  {
    name: 'Relaciones Familiares',
    program: `% Hechos
padre(juan, maria).
padre(juan, pedro).
padre(pedro, ana).

% Reglas
abuelo(X, Z) :- padre(X, Y), padre(Y, Z).`,
    query: '?- abuelo(juan, Z).'
  },
  {
    name: 'Listas - Append',
    program: `% Concatenar listas
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).`,
    query: '?- append([1,2], [3,4], X).'
  },
  {
    name: 'Miembro de Lista',
    program: `% Verificar pertenencia
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).`,
    query: '?- member(2, [1,2,3]).'
  }
];

export default function App() {
  // Estado
  const [program, setProgram] = useState(EXAMPLES[0].program);
  const [query, setQuery] = useState(EXAMPLES[0].query);
  const [strategy, setStrategy] = useState<'leftmost' | 'rightmost'>('leftmost');
  const [maxDepth, setMaxDepth] = useState(20);
  const [isResolving, setIsResolving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [solutions, setSolutions] = useState<Solution[]>([]);
  const [stats, setStats] = useState<any>(null);
  const [sidebarOpen, setSidebarOpen] = useState(true);

  // Custom hooks
  const { nodes, edges, layoutTree, clearTree } = useSLDTree();
  const [flowNodes, setFlowNodes, onNodesChange] = useNodesState(nodes);
  const [flowEdges, setFlowEdges, onEdgesChange] = useEdgesState(edges);

  // Sincronizar nodes/edges con el hook
  useEffect(() => {
    setFlowNodes(nodes);
    setFlowEdges(edges);
  }, [nodes, edges, setFlowNodes, setFlowEdges]);

  // Resolver consulta
  const handleResolve = useCallback(async () => {
    setIsResolving(true);
    setError(null);
    setSolutions([]);
    setStats(null);

    try {
      const response = await api.resolve(program, query, strategy, maxDepth);

      if (response.success && response.tree) {
        layoutTree(response.tree);
        setSolutions(response.solutions || []);
        setStats(response.stats);
      } else {
        setError(response.error || 'Error desconocido');
      }
    } catch (err) {
      setError(handleAPIError(err));
    } finally {
      setIsResolving(false);
    }
  }, [program, query, strategy, maxDepth, layoutTree]);

  // Reset
  const handleReset = useCallback(() => {
    clearTree();
    setSolutions([]);
    setStats(null);
    setError(null);
  }, [clearTree]);

  // Export
  const handleExport = useCallback(() => {
    const data = {
      program,
      query,
      strategy,
      maxDepth,
      solutions,
      stats,
      timestamp: new Date().toISOString(),
    };

    const blob = new Blob([JSON.stringify(data, null, 2)], { 
      type: 'application/json' 
    });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `sld-tree-${Date.now()}.json`;
    a.click();
    URL.revokeObjectURL(url);
  }, [program, query, strategy, maxDepth, solutions, stats]);

  // Cargar ejemplo
  const loadExample = useCallback((example: typeof EXAMPLES[0]) => {
    setProgram(example.program);
    setQuery(example.query);
    handleReset();
  }, [handleReset]);

  return (
    <div className="h-screen flex flex-col bg-gradient-to-br from-slate-950 via-purple-950 to-slate-950">
      {/* Header */}
      <motion.header 
        initial={{ y: -100 }}
        animate={{ y: 0 }}
        className="bg-black/40 backdrop-blur-md border-b border-purple-500/30 px-6 py-4 z-10"
      >
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <motion.div
              animate={{ rotate: [0, 360] }}
              transition={{ duration: 20, repeat: Infinity, ease: 'linear' }}
            >
              <Sparkles className="w-8 h-8 text-purple-400" />
            </motion.div>
            <div>
              <h1 className="text-3xl font-bold text-transparent bg-clip-text bg-gradient-to-r from-blue-400 via-purple-400 to-pink-400">
                SLD-Explorer
              </h1>
              <p className="text-sm text-gray-400">
                Visualizador interactivo de árboles SLD
              </p>
            </div>
          </div>

          <button
            onClick={() => setSidebarOpen(!sidebarOpen)}
            className="lg:hidden p-2 text-white hover:bg-white/10 rounded-lg"
          >
            {sidebarOpen ? <ChevronLeft /> : <ChevronRight />}
          </button>
        </div>
      </motion.header>

      {/* Main Content */}
      <div className="flex-1 flex overflow-hidden relative">
        {/* Sidebar */}
        <AnimatePresence>
          {sidebarOpen && (
            <motion.aside
              initial={{ x: -400, opacity: 0 }}
              animate={{ x: 0, opacity: 1 }}
              exit={{ x: -400, opacity: 0 }}
              className="w-96 bg-black/40 backdrop-blur-sm border-r border-purple-500/30 flex flex-col overflow-hidden"
            >
              {/* Examples */}
              <div className="p-4 border-b border-purple-500/30">
                <h2 className="text-lg font-bold text-white mb-3 flex items-center gap-2">
                  <BookOpen size={20} className="text-purple-400" />
                  Ejemplos
                </h2>
                <div className="flex gap-2 flex-wrap">
                  {EXAMPLES.map((ex, idx) => (
                    <motion.button
                      key={idx}
                      onClick={() => loadExample(ex)}
                      whileHover={{ scale: 1.05 }}
                      whileTap={{ scale: 0.95 }}
                      className="px-3 py-2 bg-purple-600/30 hover:bg-purple-600/50 text-purple-200 rounded-lg text-sm font-medium transition-all"
                    >
                      {ex.name}
                    </motion.button>
                  ))}
                </div>
              </div>

              {/* Program Editor */}
              <div className="flex-1 p-4 overflow-y-auto">
                <CodeEditor
                  label="Programa Prolog"
                  value={program}
                  onChange={setProgram}
                  placeholder="Escribe tu programa aquí..."
                  rows={12}
                  validate={true}
                />
              </div>

              {/* Query Editor */}
              <div className="p-4 border-t border-purple-500/30">
                <CodeEditor
                  label="Consulta"
                  value={query}
                  onChange={setQuery}
                  placeholder="?- goal(X)."
                  rows={3}
                />
              </div>

              {/* Controls */}
              <div className="p-4 border-t border-purple-500/30">
                <ControlPanel
                  onResolve={handleResolve}
                  onReset={handleReset}
                  onExport={handleExport}
                  isResolving={isResolving}
                  strategy={strategy}
                  onStrategyChange={setStrategy}
                  maxDepth={maxDepth}
                  onMaxDepthChange={setMaxDepth}
                  stats={stats}
                />
              </div>

              {/* Solutions */}
              {solutions.length > 0 && (
                <motion.div
                  initial={{ opacity: 0, y: 20 }}
                  animate={{ opacity: 1, y: 0 }}
                  className="p-4 bg-green-900/20 border-t border-green-500/30 max-h-48 overflow-y-auto"
                >
                  <h3 className="text-sm font-bold text-green-300 mb-2 flex items-center gap-2">
                    <Lightbulb size={16} />
                    Soluciones ({solutions.length})
                  </h3>
                  {solutions.map((sol, idx) => (
                    <motion.div
                      key={idx}
                      initial={{ x: -20, opacity: 0 }}
                      animate={{ x: 0, opacity: 1 }}
                      transition={{ delay: idx * 0.1 }}
                      className="bg-green-500/20 p-2 rounded-lg mb-2"
                    >
                      <div className="text-xs text-green-400 font-mono">
                        {sol.substitution}
                      </div>
                    </motion.div>
                  ))}
                </motion.div>
              )}

              {/* Error */}
              {error && (
                <motion.div
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  className="p-4 bg-red-900/20 border-t border-red-500/30"
                >
                  <div className="flex items-start gap-2">
                    <AlertCircle className="w-5 h-5 text-red-400 flex-shrink-0 mt-0.5" />
                    <div>
                      <h3 className="text-sm font-bold text-red-300 mb-1">Error</h3>
                      <p className="text-xs text-red-200 font-mono">{error}</p>
                    </div>
                    <button
                      onClick={() => setError(null)}
                      className="ml-auto text-red-400 hover:text-red-200"
                    >
                      <X size={16} />
                    </button>
                  </div>
                </motion.div>
              )}
            </motion.aside>
          )}
        </AnimatePresence>

        {/* Tree Visualization */}
        <div className="flex-1 relative">
          {flowNodes.length === 0 ? (
            <EmptyState />
          ) : (
            <ReactFlow
              nodes={flowNodes}
              edges={flowEdges}
              onNodesChange={onNodesChange}
              onEdgesChange={onEdgesChange}
              nodeTypes={nodeTypes}
              fitView
              className="bg-slate-950"
              minZoom={0.1}
              maxZoom={2}
            >
              <Background 
                color="#6366f1" 
                gap={20} 
                className="bg-slate-950"
              />
              <Controls className="bg-slate-800 border border-purple-500/30" />
              <MiniMap 
                className="bg-slate-900 border border-purple-500/30"
                nodeColor={(node) => {
                  switch (node.data.status) {
                    case 'success': return '#10b981';
                    case 'failure': return '#ef4444';
                    case 'expanded': return '#3b82f6';
                    default: return '#6b7280';
                  }
                }}
              />
              <Panel position="top-right" className="bg-slate-900/90 backdrop-blur-sm p-3 rounded-lg border border-purple-500/30">
                <Legend />
              </Panel>
            </ReactFlow>
          )}
        </div>
      </div>
    </div>
  );
}

// ============================================================================
// SUB-COMPONENTS
// ============================================================================

const EmptyState = () => (
  <div className="absolute inset-0 flex items-center justify-center">
    <motion.div
      initial={{ opacity: 0, scale: 0.9 }}
      animate={{ opacity: 1, scale: 1 }}
      className="text-center space-y-6"
    >
      <motion.div
        animate={{ 
          rotateY: [0, 360],
          scale: [1, 1.1, 1]
        }}
        transition={{ 
          duration: 3,
          repeat: Infinity,
          ease: "easeInOut"
        }}
        className="text-8xl"
      >
        🌳
      </motion.div>
      <div>
        <h3 className="text-3xl font-bold text-gray-400 mb-2">
          Árbol SLD vacío
        </h3>
        <p className="text-gray-500 text-lg">
          Carga un ejemplo o escribe tu código Prolog
        </p>
      </div>
    </motion.div>
  </div>
);

const Legend = () => (
  <div className="text-xs space-y-2">
    <div className="text-gray-300 font-bold mb-2">Leyenda:</div>
    <div className="flex items-center gap-2">
      <div className="w-3 h-3 bg-green-500 rounded-full"></div>
      <span className="text-gray-300">Éxito</span>
    </div>
    <div className="flex items-center gap-2">
      <div className="w-3 h-3 bg-red-500 rounded-full"></div>
      <span className="text-gray-300">Fallo</span>
    </div>
    <div className="flex items-center gap-2">
      <div className="w-3 h-3 bg-blue-500 rounded-full"></div>
      <span className="text-gray-300">Expandido</span>
    </div>
  </div>
);