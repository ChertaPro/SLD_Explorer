/**
 * components/CodeEditor.tsx - Editor de código Prolog
 */

import React, { useState, useEffect } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { Code2, AlertCircle, CheckCircle2, Loader2 } from 'lucide-react';
import { api, handleAPIError } from '../services/api';

interface CodeEditorProps {
  value: string;
  onChange: (value: string) => void;
  label: string;
  placeholder?: string;
  rows?: number;
  validate?: boolean;
}

export const CodeEditor: React.FC<CodeEditorProps> = ({
  value,
  onChange,
  label,
  placeholder = '',
  rows = 10,
  validate = false,
}) => {
  const [isValidating, setIsValidating] = useState(false);
  const [validationResult, setValidationResult] = useState<{
    valid: boolean;
    error: string | null;
  } | null>(null);

  // Validar en tiempo real (con debounce)
  useEffect(() => {
    if (!validate || !value.trim()) {
      setValidationResult(null);
      return;
    }

    const timeoutId = setTimeout(async () => {
      setIsValidating(true);
      try {
        const result = await api.validate(value);
        setValidationResult(result);
      } catch (error) {
        setValidationResult({
          valid: false,
          error: handleAPIError(error),
        });
      } finally {
        setIsValidating(false);
      }
    }, 1000); // Esperar 1 segundo después de escribir

    return () => clearTimeout(timeoutId);
  }, [value, validate]);

  return (
    <div className="space-y-2">
      {/* Label */}
      <div className="flex items-center justify-between">
        <label className="flex items-center gap-2 text-sm font-bold text-gray-700">
          <Code2 size={16} className="text-purple-600" />
          {label}
        </label>

        {/* Validation indicator */}
        <AnimatePresence>
          {validate && value.trim() && (
            <motion.div
              initial={{ opacity: 0, scale: 0.8 }}
              animate={{ opacity: 1, scale: 1 }}
              exit={{ opacity: 0, scale: 0.8 }}
              className="flex items-center gap-2 text-xs"
            >
              {isValidating ? (
                <div className="flex items-center gap-1 text-gray-500">
                  <Loader2 size={14} className="animate-spin" />
                  <span>Validando...</span>
                </div>
              ) : validationResult?.valid ? (
                <div className="flex items-center gap-1 text-green-600">
                  <CheckCircle2 size={14} />
                  <span>Sintaxis válida</span>
                </div>
              ) : (
                validationResult?.error && (
                  <div className="flex items-center gap-1 text-red-600">
                    <AlertCircle size={14} />
                    <span>Error de sintaxis</span>
                  </div>
                )
              )}
            </motion.div>
          )}
        </AnimatePresence>
      </div>

      {/* Editor */}
      <motion.div
        initial={{ opacity: 0, y: 10 }}
        animate={{ opacity: 1, y: 0 }}
        className="relative"
      >
        <textarea
          value={value}
          onChange={(e) => onChange(e.target.value)}
          placeholder={placeholder}
          rows={rows}
          className={`
            w-full px-4 py-3 
            bg-slate-900 text-gray-100 
            font-mono text-sm 
            rounded-lg 
            border-2 transition-all duration-200
            focus:outline-none focus:ring-2 focus:ring-purple-500/50
            resize-none
            ${
              validationResult?.valid
                ? 'border-green-500/50'
                : validationResult?.error
                ? 'border-red-500/50'
                : 'border-purple-500/30'
            }
          `}
          spellCheck={false}
        />

        {/* Line numbers (decorative) */}
        <div className="absolute left-2 top-3 text-gray-600 font-mono text-sm select-none pointer-events-none">
          {Array.from({ length: rows }, (_, i) => (
            <div key={i} className="leading-6">
              {i + 1}
            </div>
          ))}
        </div>
      </motion.div>

      {/* Error message */}
      <AnimatePresence>
        {validationResult?.error && (
          <motion.div
            initial={{ opacity: 0, height: 0 }}
            animate={{ opacity: 1, height: 'auto' }}
            exit={{ opacity: 0, height: 0 }}
            className="overflow-hidden"
          >
            <div className="p-3 bg-red-50 border-l-4 border-red-500 rounded-r-lg">
              <div className="flex items-start gap-2">
                <AlertCircle size={16} className="text-red-600 mt-0.5 flex-shrink-0" />
                <div>
                  <div className="text-sm font-semibold text-red-900">Error de Sintaxis</div>
                  <div className="text-xs text-red-700 mt-1 font-mono">
                    {validationResult.error}
                  </div>
                </div>
              </div>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  );
};