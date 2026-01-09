/**
 * services/api.ts - Cliente API para comunicarse con el backend
 */

import axios, { AxiosInstance } from 'axios';
import type {
  ParseRequest,
  ParseResponse,
  ResolveRequest,
  ResolveResponse,
  ExamplesResponse,
} from '../types/types';

// ============================================================================
// CONFIGURACIÓN
// ============================================================================

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000';

class APIClient {
  private client: AxiosInstance;

  constructor(baseURL: string) {
    this.client = axios.create({
      baseURL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // Interceptor para logging (desarrollo)
    this.client.interceptors.request.use((config) => {
      console.log(`[API] ${config.method?.toUpperCase()} ${config.url}`);
      return config;
    });

    // Interceptor para manejo de errores
    this.client.interceptors.response.use(
      (response) => response,
      (error) => {
        console.error('[API Error]', error);
        return Promise.reject(error);
      }
    );
  }

  // ==========================================================================
  // HEALTH CHECK
  // ==========================================================================

  async healthCheck(): Promise<{ status: string; service: string; version: string }> {
    const response = await this.client.get('/api/health');
    return response.data;
  }

  // ==========================================================================
  // PARSE
  // ==========================================================================

  async parse(code: string): Promise<ParseResponse> {
    const request: ParseRequest = { code };
    const response = await this.client.post<ParseResponse>('/api/parse', request);
    return response.data;
  }

  async validate(code: string): Promise<{ valid: boolean; error: string | null }> {
    const request: ParseRequest = { code };
    const response = await this.client.post<{ valid: boolean; error: string | null }>(
      '/api/validate',
      request
    );
    return response.data;
  }

  // ==========================================================================
  // RESOLVE
  // ==========================================================================

  async resolve(
    program: string,
    query: string,
    strategy: 'leftmost' | 'rightmost' = 'leftmost',
    maxDepth: number = 20
  ): Promise<ResolveResponse> {
    const request: ResolveRequest = {
      program,
      query,
      strategy,
      max_depth: maxDepth,
    };

    const response = await this.client.post<ResolveResponse>('/api/resolve', request);
    return response.data;
  }

  // ==========================================================================
  // EXAMPLES
  // ==========================================================================

  async getExamples(): Promise<ExamplesResponse> {
    const response = await this.client.get<ExamplesResponse>('/api/examples');
    return response.data;
  }

  // ==========================================================================
  // WEBSOCKET (para resolución paso a paso)
  // ==========================================================================

  createWebSocket(): WebSocket {
    const wsUrl = API_BASE_URL.replace('http', 'ws') + '/api/ws/resolve-step';
    return new WebSocket(wsUrl);
  }
}

// ============================================================================
// EXPORT SINGLETON
// ============================================================================

export const api = new APIClient(API_BASE_URL);

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

export const handleAPIError = (error: any): string => {
  if (axios.isAxiosError(error)) {
    if (error.response) {
      // Error del servidor
      return error.response.data?.detail || error.response.data?.error || 'Error del servidor';
    } else if (error.request) {
      // No hubo respuesta
      return 'No se pudo conectar con el servidor. ¿Está corriendo en el puerto 8000?';
    }
  }
  return error.message || 'Error desconocido';
};