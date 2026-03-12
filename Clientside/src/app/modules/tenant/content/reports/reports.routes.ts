import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { ReportsDashboardComponent } from './pages/reports-dashboard/reports-dashboard.component';

export const REPORTS_ROUTES: Routes = [
{
  path: '',
  canMatch: [roleAtLeast('MANAGER')],
  component: ReportsDashboardComponent
}
];