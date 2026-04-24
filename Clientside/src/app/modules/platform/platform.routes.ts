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

      /* =========================
         DASHBOARD
      ========================= */

      {
        path: '',
        data: {
          title: 'Dashboard'
        },
        loadComponent: () =>
          import('./content/platform-dashboard/platform-dashboard.component')
            .then(m => m.PlatformDashboardComponent)
      },

      /* =========================
         DEVICES
      ========================= */

      {
        path: 'security/devices',
        data: {
          title: 'Devices'
        },
        loadComponent: () =>
          import('./content/devices/platform-devices.component')
            .then(m => m.PlatformDevicesComponent)
      },

      /* =========================
         TENANTS
      ========================= */

      {
        path: 'tenants',
        data: {
          title: 'Tenants'
        },
        loadChildren: () =>
          import('./content/tenants/tenants.routes')
            .then(m => m.TENANT_ROUTES)
      },

      /* =========================
         PLANS
      ========================= */

      {
        path: 'plans',
        data: {
          title: 'Plans'
        },
        loadChildren: () =>
          import('./content/plans/plans.routes')
            .then(m => m.PLAN_ROUTES)
      },

      {
        path: 'users',
        data: {
          title: 'Platform Users'
        },
        loadChildren: () =>
          import('./content/users/users.routes')
            .then(m => m.USER_ROUTES)
      },

      /* =========================
         TENANT SUBSCRIPTION
      ========================= */

      {
        path: 'tenants/:id/subscription',
        data: {
          title: 'Tenant Subscription'
        },
        loadComponent: () =>
          import('./content/tenants/pages/tenant-subscription/tenant-subscription.component')
            .then(m => m.TenantSubscriptionComponent)
      },

      /* =========================
         ANALYTICS
      ========================= */

      {
        path: 'analytics',
        data: {
          title: 'Analytics'
        },
        loadComponent: () =>
          import('./content/platform-analytics/platform-analytics.component')
            .then(m => m.PlatformAnalyticsComponent)
      },

      /* =========================
         AUDIT
      ========================= */

      {
        path: 'audit',
        data: {
          title: 'Audit Logs'
        },
        loadComponent: () =>
          import('./content/platform-audit/platform-audit.component')
            .then(m => m.PlatformAuditComponent)
      }

    ]

  }

];