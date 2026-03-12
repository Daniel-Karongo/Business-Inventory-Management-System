import { Routes } from '@angular/router';

import { platformModeGuard } from './core/guards/platform-mode.guard';
import { tenantModeGuard } from './core/guards/tenant-mode.guard';

export const routes: Routes = [

  /* ============================================================
     ROOT ENTRY
  ============================================================ */

  {
    path: '',
    pathMatch: 'full',
    redirectTo: 'auth'
  },

  /* ============================================================
     AUTH MODULE (PUBLIC)
  ============================================================ */

  {
    path: 'auth',
    loadChildren: () =>
      import('./modules/auth/auth.routes')
        .then(m => m.AUTH_ROUTES)
  },

  /* ============================================================
     PLATFORM MODE
     platform.localhost:4200/platform
  ============================================================ */

  {
    path: 'platform',
    canMatch: [platformModeGuard],
    loadChildren: () =>
      import('./modules/platform/platform.routes')
        .then(m => m.PLATFORM_ROUTES)
  },

  /* ============================================================
     TENANT MODE
     default.localhost:4200/app
  ============================================================ */

  {
    path: 'app',
    canMatch: [tenantModeGuard],
    loadChildren: () =>
      import('./modules/tenant/tenant.routes')
        .then(m => m.TENANT_ROUTES)
  },

  /* ============================================================
     FALLBACK
  ============================================================ */

  {
    path: '**',
    redirectTo: 'auth'
  }

];