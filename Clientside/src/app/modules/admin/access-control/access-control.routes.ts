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
      }
      //   // placeholders — wired in later steps
      //   {
      //     path: 'roles',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'permissions',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'role-matrix',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'endpoints',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'conditions',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'branch-scopes',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   },
      //   {
      //     path: 'audit-log',
      //     loadComponent: () =>
      //       import('./pages/overview/acl-overview.component')
      //         .then(m => m.AclOverviewComponent)
      //   }
    ]
  }
];