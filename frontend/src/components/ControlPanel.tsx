/**
 * components/ControlPanel.tsx - Panel de control principal
 */

import React from 'react';
import { motion } from 'framer-motion';
import { Play, RotateCcw, Settings, Zap } from 'lucide-react';
import type { Stats } from '../types/types';

interface ControlPanelProps {
  onResolve: () => void;
  onReset: () => void;
  isResolving: boolean;
  strategy: 'leftmost' | 'rightmost';
  onStrategyChange: (strategy: 'leftmost' | 'rightmost') => void;
  maxDepth: number;
  onMaxDepthChange: (depth: number) => void;
  stats: Stats | null;
}

export const ControlPanel: React.FC<ControlPanelProps> = ({
  onResolve,
  onReset,
  isResolving,
  strategy,
  onStrategyChange,
  maxDepth,
  onMaxDepthChange,
  stats,
}) => {
  return (
    <div className="space-y-4">
      {/* Main Actions */}
      <div className="space-y-3">
        <motion.button
          onClick={onResolve}
          disabled={isResolving}
          whileHover={{ scale: 1.02 }}
          whileTap={{ scale: 0.98 }}
          className={`
            w-full py-4 px-6
            bg-gradient-to-r from-blue-600 to-purple-600
            hover:from-blue-700 hover:to-purple-700
            disabled:from-gray-500 disabled:to-gray-600
            text-white font-bold text-lg
            rounded-xl shadow-lg
            transition-all duration-200
            flex items-center justify-center gap-3
            ${isResolving ? 'cursor-not-allowed' : 'cursor-pointer'}
          `}
        >
          {isResolving ? (
            <>
              <motion.div
                animate={{ rotate: 360 }}
                transition={{ duration: 1, repeat: Infinity, ease: 'linear' }}
              >
                <Zap className="w-6 h-6" />
              </motion.div>
              <span>Resolviendo...</span>
            </>
          ) : (
            <>
              <Play className="w-6 h-6" />
              <span>Resolver Consulta</span>
            </>
          )}
        </motion.button>

        <motion.button
          onClick={onReset}
          whileHover={{ scale: 1.02 }}
          whileTap={{ scale: 0.98 }}
          className="
            w-full py-3 px-4
            bg-slate-700 hover:bg-slate-600
            text-white font-semibold
            rounded-lg shadow-md
            transition-all duration-200
            flex items-center justify-center gap-2
          "
        >
          <RotateCcw size={18} />
          <span>Reset</span>
        </motion.button>
      </div>

      {/* Settings */}
      <div className="p-4 bg-slate-800 rounded-xl space-y-4">
        <div className="flex items-center gap-2 text-white font-bold">
          <Settings size={18} className="text-purple-400" />
          <span>Configuración</span>
        </div>

        {/* Strategy */}
        <div className="space-y-2">
          <label className="text-sm font-semibold text-gray-300">
            Estrategia de Selección
          </label>
          <div className="grid grid-cols-2 gap-2">
            <button
              onClick={() => onStrategyChange('leftmost')}
              className={`
                py-2 px-3 rounded-lg font-medium text-sm
                transition-all duration-200
                ${
                  strategy === 'leftmost'
                    ? 'bg-purple-600 text-white shadow-lg'
                    : 'bg-slate-700 text-gray-300 hover:bg-slate-600'
                }
              `}
            >
              Leftmost
            </button>
            <button
              onClick={() => onStrategyChange('rightmost')}
              className={`
                py-2 px-3 rounded-lg font-medium text-sm
                transition-all duration-200
                ${
                  strategy === 'rightmost'
                    ? 'bg-purple-600 text-white shadow-lg'
                    : 'bg-slate-700 text-gray-300 hover:bg-slate-600'
                }
              `}
            >
              Rightmost
            </button>
          </div>
        </div>

        {/* Max Depth */}
        <div className="space-y-2">
          <div className="flex items-center justify-between">
            <label className="text-sm font-semibold text-gray-300">
              Profundidad Máxima
            </label>
            <span className="text-lg font-bold text-purple-400">{maxDepth}</span>
          </div>
          <input
            type="range"
            min="5"
            max="50"
            step="5"
            value={maxDepth}
            onChange={(e) => onMaxDepthChange(Number(e.target.value))}
            className="w-full h-2 bg-slate-700 rounded-lg appearance-none cursor-pointer accent-purple-600"
          />
          <div className="flex justify-between text-xs text-gray-500">
            <span>5</span>
            <span>50</span>
          </div>
        </div>
      </div>

      {/* Stats */}
      {stats && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          className="p-4 bg-gradient-to-br from-slate-800 to-slate-900 rounded-xl"
        >
          <div className="text-white font-bold mb-3 flex items-center gap-2">
            <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse" />
            <span>Estadísticas</span>
          </div>
          <div className="grid grid-cols-2 gap-3">
            <StatCard
              label="Nodos"
              value={stats.total_nodes}
              color="bg-blue-500"
            />
            <StatCard
              label="Soluciones"
              value={stats.solutions_found}
              color="bg-green-500"
            />
            <StatCard
              label="Éxitos"
              value={stats.success_nodes}
              color="bg-emerald-500"
            />
            <StatCard
              label="Fallos"
              value={stats.failure_nodes}
              color="bg-red-500"
            />
          </div>
        </motion.div>
      )}
    </div>
  );
};

// ============================================================================
// SUB-COMPONENTS
// ============================================================================

interface StatCardProps {
  label: string;
  value: number;
  color: string;
}

const StatCard: React.FC<StatCardProps> = ({ label, value, color }) => (
  <motion.div
    initial={{ scale: 0.9, opacity: 0 }}
    animate={{ scale: 1, opacity: 1 }}
    className={`${color}/20 p-3 rounded-lg border border-${color}/30`}
  >
    <div className="text-xs text-gray-400 font-medium">{label}</div>
    <div className={`text-2xl font-bold text-white mt-1`}>{value}</div>
  </motion.div>
);