import { MeResponse } from '../../modules/auth/services/auth.service';

/**
 * Roles that are allowed to see deleted entities
 */
const ROLES_CAN_SEE_DELETED = new Set([
  'SUPERUSER',
  'ADMIN',
  'MANAGER'
]);

export function canSeeDeleted(me: MeResponse | null): boolean {
  return !!me && ROLES_CAN_SEE_DELETED.has(me.role);
}