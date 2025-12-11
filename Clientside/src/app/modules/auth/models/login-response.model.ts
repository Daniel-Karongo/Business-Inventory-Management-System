export interface LoginResponse {
  token: string;
  refreshToken?: string;
  expiresAt: number;
  role: string;
}