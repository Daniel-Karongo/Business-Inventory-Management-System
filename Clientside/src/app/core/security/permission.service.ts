import { Injectable, inject } from '@angular/core';
import { AuthService } from '../../modules/auth/services/auth.service';
import { ROLE_PERMISSIONS, Feature } from './role-permissions';

@Injectable({ providedIn: 'root' })
export class PermissionService {

  private auth = inject(AuthService);

  can(feature: Feature | string): boolean {

    const role = this.auth.getSnapshot()?.role ?? '';

    const permissions = ROLE_PERMISSIONS[role] ?? [];

    if (permissions.includes('*')) return true;

    return permissions.includes(feature as Feature);
  }

}