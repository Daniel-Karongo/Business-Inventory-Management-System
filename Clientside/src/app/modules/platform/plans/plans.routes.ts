import { Routes } from '@angular/router';

import { PlanListComponent } from './pages/plan-list/plan-list.component';
import { PlanCreateComponent } from './pages/plan-create/plan-create.component';
import { platformModeGuard } from '../../../core/guards/platform-mode.guard';


export const PLAN_ROUTES: Routes = [

  {
    path: '',
    component: PlanListComponent,
    canActivate: [platformModeGuard]
  },

  {
    path: 'create',
    component: PlanCreateComponent,
    canActivate: [platformModeGuard]
  }

];