import { Routes } from '@angular/router';

import { PlanListComponent } from './pages/plan-list/plan-list.component';
import { PlanCreateComponent } from './pages/plan-create/plan-create.component';

import { platformAdminGuard } from '../guards/platform-admin.guard';

export const PLAN_ROUTES: Routes = [

  {
    path: '',
    component: PlanListComponent,
    canActivate: [platformAdminGuard]
  },

  {
    path: 'create',
    component: PlanCreateComponent,
    canActivate: [platformAdminGuard]
  }

];