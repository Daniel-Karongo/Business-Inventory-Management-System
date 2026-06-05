import {
  MinimalUserDTO
} from '../../users/models/user.model';

/* =========================================================
   CORE
========================================================= */

export interface BranchListItemDTO {
  id: string;
  branchCode: string;
  name: string;
  location?: string;
  deleted: boolean;
  createdAt?: string;
  email: string;
  phone: string;
}

/* =========================================================
   LEGACY COMPATIBILITY DTO
   TEMPORARY MIGRATION BRIDGE
========================================================= */

export type BranchDTO = BranchListItemDTO;
export interface BranchDetailsDTO {
  id: string;
  branchCode: string;
  name: string;
  location?: string;
  phone?: string;
  email?: string;
  latitude?: number;
  longitude?: number;
  radiusMeters?: number;
  enforceGeofence?: boolean;
  enforceDevice?: boolean;
  maxActiveSessionsPerUser: number;
  rollcallStartTime?: string;
  rollcallGraceMinutes?: number;
  logoutTime?: string;
  deleted?: boolean;
  createdAt?: string;
  users?: MinimalUserDTO[];
}

/* =========================================================
   CREATE / UPDATE
========================================================= */

export interface BranchFormDTO {
  branchCode: string;
  name: string;
  location?: string;
  phone?: string;
  email?: string;
  latitude?: number | null;
  longitude?: number | null;
  radiusMeters?: number | null;
  enforceGeofence?: boolean;
  enforceDevice?: boolean;
  maxActiveSessionsPerUser: number;
  rollcallStartTime?: string | null;
  rollcallGraceMinutes?: number | null;
  logoutTime?: string | null;
  userIds?: string[];
}

/* =========================================================
   SECURITY
========================================================= */

export interface BranchSecuritySettingsDTO {
  latitude?: number | null;
  longitude?: number | null;
  radiusMeters?: number | null;
  enforceGeofence: boolean;
  enforceDevice: boolean;
  maxActiveSessionsPerUser: number;
}

/* =========================================================
   ATTENDANCE
========================================================= */

export interface BranchAttendanceSettingsDTO {
  rollcallStartTime?: string | null;
  rollcallGraceMinutes?: number | null;
  logoutTime?: string | null;
}

/* =========================================================
   NOTIFICATIONS
========================================================= */

export interface BranchNotificationSettingsDTO {
  smsEnabled: boolean;
  emailEnabled: boolean;
  allowRetries: boolean;
  maxRetryCount: number;
}

/* =========================================================
   EMAIL
========================================================= */

export interface BranchEmailSettingsDTO {
  enabled: boolean;
  host: string;
  port: number;
  username: string;
  password?: string;
  fromAddress: string;
  authEnabled: boolean;
  tlsEnabled: boolean;
  active: boolean;
}

/* =========================================================
   SMS
========================================================= */

export interface BranchSmsSettingsDTO {
  enabled: boolean;
  provider: string;
  username: string;
  apiKey?: string;
  senderId: string;
  defaultCountryCode: string;
  sandbox: boolean;
  active: boolean;
}

/* =========================================================
   MPESA
========================================================= */

export interface BranchMpesaSettingsDTO {
  enabled: boolean;
  sandbox: boolean;
  shortcode: string;
  consumerKey?: string;
  consumerSecret?: string;
  passkey?: string;
  securityCredential?: string;
  stkCallbackUrl: string;
  c2bValidationUrl?: string;
  c2bConfirmationUrl?: string;
  initiatorName: string;
  active: boolean;
}

/* =========================================================
   EMAIL RESPONSE
========================================================= */

export interface BranchEmailSettingsResponseDTO
  extends Omit<BranchEmailSettingsDTO, 'password'> {
  password: string;
}

/* =========================================================
   SMS RESPONSE
========================================================= */

export interface BranchSmsSettingsResponseDTO
  extends Omit<BranchSmsSettingsDTO, 'apiKey'> {
  apiKey: string;
}

/* =========================================================
   MPESA RESPONSE
========================================================= */

export interface BranchMpesaSettingsResponseDTO
  extends Omit<
    BranchMpesaSettingsDTO,
    | 'consumerKey'
    | 'consumerSecret'
    | 'passkey'
    | 'securityCredential'
  > {
  consumerKey: string;
  consumerSecret: string;
  passkey: string;
  securityCredential: string;
}

/* =========================================================
   TABLE STATE
========================================================= */

export interface BranchTableState {
  search: string;
  includeDeleted: boolean;
  sortBy: string;
  sortDirection: 'asc' | 'desc';
  page: number;
  size: number;
}

/* =========================================================
   HIERARCHY
========================================================= */

export interface BranchHierarchyDTO {
  branchId: string;
  branchName: string;
  primaryBranch: boolean;
}

/* =========================================================
   MINIMAL
========================================================= */

export interface BranchMinimalDTO {
  id: string;
  branchCode: string;
  name: string;
}

/* =========================================================
   BULK IMPORT
========================================================= */

export interface BranchBulkRow {
  branchCode: string;
  name: string;
  location?: string;
  phone?: string;
  email?: string;
}

/* =========================================================
   AUDITS
========================================================= */

export interface BranchAuditDTO {
  id: string;
  branchId: string;
  branchName: string;
  action: string;
  fieldChanged?: string;
  oldValue?: string;
  newValue?: string;
  reason?: string;
  performedById?: string;
  performedByUsername?: string;
  timestamp: string;
}
export interface BranchAuditTableState {
  page: number;
  size: number;
  search?: string;
}