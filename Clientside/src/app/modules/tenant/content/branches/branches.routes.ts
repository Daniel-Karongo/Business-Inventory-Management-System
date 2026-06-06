import { Routes } from '@angular/router';

import {
  roleAtLeast
} from '../../../../core/security/role-at-least.guard';

import {
  BranchCreateComponent
} from './pages/branch-create/branch-create.component';

import {
  BranchEditComponent
} from './pages/branch-edit/branch-edit.component';

import {
  BranchListComponent
} from './pages/branch-list/branch-list.component';

import {
  BranchDetailsComponent
} from './pages/branch-details/branch-details.component';

import {
  BranchWorkspaceComponent
} from './pages/branch-workspace/branch-workspace.component';

import {
  BranchSecurityComponent
} from './pages/branch-security/branch-security.component';

import {
  BranchAttendanceComponent
} from './pages/branch-attendance/branch-attendance.component';

import {
  BranchNotificationsComponent
} from './pages/branch-notifications/branch-notifications.component';

import {
  BranchEmailComponent
} from './pages/branch-email/branch-email.component';

import {
  BranchSmsComponent
} from './pages/branch-sms/branch-sms.component';

import {
  BranchMpesaComponent
} from './pages/branch-mpesa/branch-mpesa.component';

import {
  BranchAuditsComponent
} from './pages/branch-audits/branch-audits.component';
import { BranchDocumentsComponent } from './pages/branch-documents/branch-documents.component';

export const BRANCH_ROUTES: Routes = [
  {
    path: '',
    canMatch: [
      roleAtLeast('MANAGER')
    ],
    children: [

      /* ===================================================
         LIST
      ==================================================== */

      {
        path: '',
        component: BranchListComponent
      },

      /* ===================================================
         CREATE
      ==================================================== */

      {
        path: 'create',
        component: BranchCreateComponent
      },

      /* ===================================================
         EDIT
      ==================================================== */

      {
        path: ':id/edit',
        component: BranchEditComponent
      },

      /* ===================================================
         WORKSPACE
      ==================================================== */

      {
        path: ':id',

        component: BranchWorkspaceComponent,

        children: [

          {
            path: '',
            pathMatch: 'full',
            redirectTo: 'overview'
          },

          {
            path: 'images',
            component: BranchDocumentsComponent
          },

          {
            path: 'overview',
            component: BranchDetailsComponent
          },

          {
            path: 'security',
            component: BranchSecurityComponent
          },

          {
            path: 'attendance',
            component: BranchAttendanceComponent
          },

          {
            path: 'notifications',
            component: BranchNotificationsComponent
          },

          {
            path: 'email',
            component: BranchEmailComponent
          },

          {
            path: 'sms',
            component: BranchSmsComponent
          },

          {
            path: 'mpesa',
            component: BranchMpesaComponent
          },

          {
            path: 'audits',
            component: BranchAuditsComponent
          }
        ]
      }
    ]
  }
];