import { Routes } from '@angular/router';
import { platformModeGuard } from '../../core/guards/platform-mode.guard';

export const PLATFORM_ROUTES: Routes = [

  {
    path: '',
    canMatch: [platformModeGuard],
    loadComponent: () =>
      import('./layout/platform-layout.component')
        .then(m => m.PlatformLayoutComponent),

    children: [

      {
        path: '',
        loadComponent: () =>
          import('./pages/platform-dashboard/platform-dashboard.component')
            .then(m => m.PlatformDashboardComponent)
      },

      {
        path: 'tenants',
        loadChildren: () =>
          import('./tenants/tenants.routes')
            .then(m => m.TENANT_ROUTES)
      },

      {
        path: 'plans',
        loadChildren: () =>
          import('./plans/plans.routes')
            .then(m => m.PLAN_ROUTES)
      },

      {
        path: 'tenants/:id/subscription',
        loadComponent: () =>
          import('./tenants/pages/tenant-subscription/tenant-subscription.component')
            .then(m => m.TenantSubscriptionComponent)
      },

      {
        path: 'analytics',
        loadComponent: () =>
          import('./pages/platform-analytics/platform-analytics.component')
            .then(m => m.PlatformAnalyticsComponent)
      },

      {
        path: 'audit',
        loadComponent: () =>
          import('./pages/platform-audit/platform-audit.component')
            .then(m => m.PlatformAuditComponent)
      }

    ]

  }

];