export interface ApiResponse<T = any> {
  status: string;   // "success", "error"
  message: string;  // readable message
  data?: T;         // optional payload
}