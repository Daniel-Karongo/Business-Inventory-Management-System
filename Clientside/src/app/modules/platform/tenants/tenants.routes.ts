import { Routes } from '@angular/router';
import { TenantListComponent } from './pages/tenant-list/tenant-list.component';
import { TenantCreateComponent } from './pages/tenant-create/tenant-create.component';
import { platformAdminGuard } from '../guards/platform-admin.guard';

export const TENANT_ROUTES: Routes = [

  {
    path: '',
    component: TenantListComponent,
    canActivate: [platformAdminGuard]
  },

  {
    path: 'create',
    component: TenantCreateComponent,
    canActivate: [platformAdminGuard]
  },

  {
    path: ':id',
    canActivate: [platformAdminGuard],
    loadComponent: () =>
      import('./pages/tenant-detail/tenant-detail.component')
        .then(m => m.TenantDetailComponent)
  }

];