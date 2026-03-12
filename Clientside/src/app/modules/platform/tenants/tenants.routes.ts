import { Routes } from '@angular/router';
import { TenantListComponent } from './pages/tenant-list/tenant-list.component';
import { TenantCreateComponent } from './pages/tenant-create/tenant-create.component';
import { platformModeGuard } from '../../../core/guards/platform-mode.guard';

export const TENANT_ROUTES: Routes = [

  {
    path: '',
    component: TenantListComponent,
    canActivate: [platformModeGuard]
  },

  {
    path: 'create',
    component: TenantCreateComponent,
    canActivate: [platformModeGuard]
  },

  {
    path: ':id',
    canActivate: [platformModeGuard],
    loadComponent: () =>
      import('./pages/tenant-detail/tenant-detail.component')
        .then(m => m.TenantDetailComponent)
  }

];