export interface TenantResponse {
  id: string;
  name: string;
  code: string;
  status: 'ACTIVE' | 'TRIAL' | 'SUSPENDED' | 'EXPIRED' | 'DELETED';
  platformTenant: boolean;
  createdAt: string;
}

export interface TenantCreateRequest {
  name: string;
  code: string;
  adminUsername?: string;
  adminPassword?: string;
}