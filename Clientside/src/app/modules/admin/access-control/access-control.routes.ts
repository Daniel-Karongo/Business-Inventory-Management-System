import { Routes } from '@angular/router';
import { AccessControlLayoutComponent } from './layout/access-control-layout/access-control-layout.component';

export const ACCESS_CONTROL_ROUTES: Routes = [
  {
    path: '',
    component: AccessControlLayoutComponent,
    children: [
      { path: '', redirectTo: 'overview', pathMatch: 'full' },

      {
        path: 'overview',
        loadComponent: () =>
          import('./pages/overview/acl-overview/acl-overview.component')
            .then(m => m.AclOverviewComponent)
      },
      {
        path: 'roles',
        loadComponent: () =>
          import('./pages/roles/acl-role-matrix/acl-role-matrix.component')
            .then(m => m.AclRoleMatrixComponent)
      },
      {
        path: 'permissions',
        loadComponent: () =>
          import('./pages/permissions/acl-permission-conditions/acl-permission-conditions.component')
            .then(m => m.AclPermissionConditionsComponent)
      },
      {
        path: 'endpoints',
        loadComponent: () =>
          import('./pages/endpoints/acl-endpoint-permissions/acl-endpoint-permissions.component')
            .then(m => m.AclEndpointPermissionsComponent)
      }
    ]
  }
];