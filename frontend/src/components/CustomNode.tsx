/**
 * components/CustomNode.tsx - Nodo personalizado para React Flow
 * Versión mejorada con UMG, SRC y Cláusula usada
 */

import React from 'react';
import { Handle, Position } from 'reactflow';
import { motion } from 'framer-motion';
import { CheckCircle2, XCircle, ArrowRight, Circle, FileCode, Zap, Sparkles } from 'lucide-react';
import type { CustomNodeData } from '../types/types';

interface CustomNodeProps {
  data: CustomNodeData;
}

export const CustomNode: React.FC<CustomNodeProps> = ({ data }) => {
  // Icono según estado
  const getStatusIcon = () => {
    switch (data.status) {
      case 'success':
        return <CheckCircle2 className="w-5 h-5 text-green-600" />;
      case 'failure':
        return <XCircle className="w-5 h-5 text-red-600" />;
      case 'expanded':
        return <ArrowRight className="w-5 h-5 text-blue-600" />;
      default:
        return <Circle className="w-5 h-5 text-gray-400" />;
    }
  };

  // Colores según estado
  const getColors = () => {
    switch (data.status) {
      case 'success':
        return {
          bg: 'from-green-50 via-emerald-50 to-green-100',
          border: 'border-green-500',
          text: 'text-green-900',
          badge: 'bg-green-500',
          glow: 'bg-green-500',
        };
      case 'failure':
        return {
          bg: 'from-red-50 via-rose-50 to-red-100',
          border: 'border-red-500',
          text: 'text-red-900',
          badge: 'bg-red-500',
          glow: 'bg-red-500',
        };
      case 'expanded':
        return {
          bg: 'from-blue-50 via-cyan-50 to-blue-100',
          border: 'border-blue-500',
          text: 'text-blue-900',
          badge: 'bg-blue-500',
          glow: 'bg-blue-500',
        };
      default:
        return {
          bg: 'from-gray-50 via-slate-50 to-gray-100',
          border: 'border-gray-400',
          text: 'text-gray-900',
          badge: 'bg-gray-500',
          glow: 'bg-gray-500',
        };
    }
  };

  const colors = getColors();

  return (
    <>
      <Handle type="target" position={Position.Top} className="w-3 h-3 !bg-purple-500" />
      
      <motion.div
        initial={{ scale: 0, opacity: 0 }}
        animate={{ scale: 1, opacity: 1 }}
        transition={{
          type: 'spring',
          stiffness: 260,
          damping: 20,
        }}
        whileHover={{ scale: 1.05 }}
        className={`
          relative min-w-[300px] max-w-[380px]
          bg-gradient-to-br ${colors.bg}
          border-2 ${colors.border}
          rounded-xl shadow-lg hover:shadow-2xl
          transition-all duration-300
          ${data.isAnimating ? 'animate-pulse-slow' : ''}
        `}
      >
        {/* Header */}
        <div className="p-4 border-b border-gray-200">
          <div className="flex items-center justify-between mb-2">
            <div className="flex items-center gap-2">
              {getStatusIcon()}
              <span className="text-xs font-bold text-gray-600 uppercase tracking-wider">
                {data.status}
              </span>
            </div>
            <div className={`px-2 py-1 rounded-full ${colors.badge} text-white text-xs font-bold`}>
              Depth: {data.depth}
            </div>
          </div>
        </div>

        {/* Body - Goals */}
        <div className="p-4">
          {data.goals && data.goals.length > 0 ? (
            <div className="space-y-2">
              <div className="text-xs font-bold text-gray-700 uppercase mb-2">Goals:</div>
              <div className="space-y-1 max-h-[200px] overflow-y-auto custom-scrollbar">
                {data.goals.map((goal, idx) => (
                  <motion.div
                    key={idx}
                    initial={{ x: -10, opacity: 0 }}
                    animate={{ x: 0, opacity: 1 }}
                    transition={{ delay: idx * 0.05 }}
                    className={`
                      text-sm font-mono px-3 py-2 rounded-lg
                      ${
                        idx === data.selected_goal_index
                          ? 'bg-blue-200 border-2 border-blue-500 font-bold'
                          : 'bg-white/60 border border-gray-300'
                      }
                    `}
                  >
                    {idx === data.selected_goal_index && (
                      <span className="text-blue-600 mr-2">▶</span>
                    )}
                    {goal}
                  </motion.div>
                ))}
              </div>
            </div>
          ) : (
            <motion.div
              initial={{ scale: 0 }}
              animate={{ scale: 1 }}
              className="text-center py-4"
            >
              <div className="text-4xl mb-2">✅</div>
              <div className="text-lg font-bold text-green-700">Success!</div>
              <div className="text-xs text-green-600 mt-1">Goals satisfechos</div>
            </motion.div>
          )}
        </div>

        {/* UMG (Unificador Más General) - Para todos los nodos excepto el raíz */}
        {data.umg && data.umg !== '{}' && (
          <motion.div
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            className="px-4 pb-3"
          >
            <div className="bg-purple-50 p-3 rounded-lg border-2 border-purple-300">
              <div className="flex items-center gap-2 mb-2">
                <Zap className="w-4 h-4 text-purple-600" />
                <span className="text-xs font-bold text-purple-700">
                  UMG{data.depth} (Unificación)
                </span>
              </div>
              <div className="text-xs font-mono text-purple-900 bg-white/70 px-3 py-2 rounded-lg break-all">
                {data.umg}
              </div>
            </div>
          </motion.div>
        )}

        {/* SRC (Substitución Resultado Computado) - Solo para nodos SUCCESS */}
        {data.status === 'success' && data.src && data.src !== '{}' && (
          <motion.div
            initial={{ opacity: 0, scale: 0.9 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ delay: 0.2 }}
            className="px-4 pb-3"
          >
            <div className="bg-gradient-to-r from-green-100 to-emerald-100 p-4 rounded-lg border-2 border-green-400 shadow-md">
              <div className="flex items-center gap-2 mb-2">
                <motion.div
                  animate={{ rotate: [0, 360] }}
                  transition={{ duration: 2, repeat: Infinity, ease: 'linear' }}
                >
                  <Sparkles className="w-5 h-5 text-green-600" />
                </motion.div>
                <span className="text-sm font-bold text-green-800">
                  SRC (Solución Final)
                </span>
              </div>
              <div className="text-sm font-mono font-bold text-green-900 bg-white/80 px-4 py-3 rounded-lg border border-green-300 break-all">
                {data.src}
              </div>
            </div>
          </motion.div>
        )}

        {/* Cláusula Usada */}
        {data.clause_used && (
          <motion.div
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.1 }}
            className="p-4 bg-indigo-50 border-t border-indigo-200 rounded-b-xl"
          >
            <div className="flex items-center gap-2 mb-2">
              <FileCode className="w-4 h-4 text-indigo-600" />
              <span className="text-xs font-bold text-indigo-700">Cláusula Aplicada:</span>
            </div>
            <div className="text-xs font-mono text-indigo-900 bg-white/70 px-3 py-2 rounded-lg border border-indigo-300 break-all">
              {data.clause_used}
            </div>
          </motion.div>
        )}

        {/* Glow effect para nodos importantes */}
        {(data.status === 'success' || data.status === 'failure') && (
          <motion.div
            className={`
              absolute -inset-1 rounded-xl blur-lg opacity-30
              ${colors.glow}
            `}
            animate={{
              opacity: [0.3, 0.6, 0.3],
            }}
            transition={{
              duration: 2,
              repeat: Infinity,
            }}
            style={{ zIndex: -1 }}
          />
        )}
      </motion.div>

      <Handle type="source" position={Position.Bottom} className="w-3 h-3 !bg-purple-500" />
    </>
  );
};

// Estilos para scrollbar personalizado
const styles = `
  .custom-scrollbar::-webkit-scrollbar {
    width: 6px;
  }
  .custom-scrollbar::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 10px;
  }
  .custom-scrollbar::-webkit-scrollbar-thumb {
    background: #888;
    border-radius: 10px;
  }
  .custom-scrollbar::-webkit-scrollbar-thumb:hover {
    background: #555;
  }
`;

// Inyectar estilos
if (typeof document !== 'undefined') {
  const styleSheet = document.createElement('style');
  styleSheet.innerText = styles;
  document.head.appendChild(styleSheet);
}