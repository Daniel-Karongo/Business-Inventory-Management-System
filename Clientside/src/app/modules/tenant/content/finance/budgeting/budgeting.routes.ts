import { Routes } from '@angular/router';
import { BudgetDashboardComponent } from './pages/budget-dashboard/budget-dashboard.component';
import { BudgetVarianceComponent } from './pages/budget-variance/budget-variance.component';

export const BUDGETING_ROUTES: Routes = [
  { path: '', component: BudgetDashboardComponent },
  { path: 'variance', component: BudgetVarianceComponent }
];