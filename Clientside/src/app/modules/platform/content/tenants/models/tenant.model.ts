export interface TenantResponse {
 id:string;
 name:string;
 code:string;
 status:
  | 'ACTIVE'
  | 'TRIAL'
  | 'SUSPENDED'
  | 'EXPIRED'
  | 'DELETED';
 createdAt:string;
 planCode:string;
 subscriptionStatus:string;
 platformTenant:boolean;
}

export interface TenantPage {
  content: TenantResponse[];
  totalElements: number;
  totalPages: number;
  pageNumber: number;
  pageSize: number;
  last: boolean;
}

export interface TenantCreateRequest {
  name: string;
  code: string;
  adminUsername?: string;
  adminPassword?: string;
}