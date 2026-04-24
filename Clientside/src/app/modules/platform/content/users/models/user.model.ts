export interface PlatformUser {
  id: string;
  username: string;
  role: 'PLATFORM_ADMIN' | 'PLATFORM_SUPER_ADMIN';
  active: boolean;
  locked: boolean;
  mustChangePassword: boolean;
  emailAddresses: string[];
  phoneNumbers: string[];
  idNumber: string | null;
  createdAt: string;
  updatedAt: string | null;
}

export interface PlatformUserPage {
  content: PlatformUser[];
  totalElements: number;
  totalPages: number;
  pageNumber: number;
  pageSize: number;
  last: boolean;
}

export interface PlatformUserCreateRequest {
  username: string;
  password: string;
  role: 'PLATFORM_ADMIN' | 'PLATFORM_SUPER_ADMIN';
}

export interface PlatformUserUpdateRequest {
  username: string;
  role: 'PLATFORM_ADMIN' | 'PLATFORM_SUPER_ADMIN';
  active: boolean;
  locked: boolean;
  mustChangePassword: boolean;
  emailAddresses: string[];
  phoneNumbers: string[];
  idNumber: string | null;
  password?: string;
}

export interface PlatformUserAudit {
  id: string;
  userId: string;
  username: string;
  action: 'CREATE'|'UPDATE'|'LOCK'|'UNLOCK'|'DELETE'|'PASSWORD_CHANGE';
  reason: string;
  performedBy: string;
  timestamp: string;
}

export interface PlatformUserAuditPage {
  content: PlatformUserAudit[];
  totalElements: number;
  totalPages: number;
  pageNumber: number;
  pageSize: number;
  last: boolean;
}