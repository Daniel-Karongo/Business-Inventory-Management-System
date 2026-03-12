import { Routes } from '@angular/router';

import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { DepartmentCreateComponent } from './pages/department-create/department-create.component';
import { DepartmentDetailsComponent } from './pages/department-details/department-details.component';
import { DepartmentEditComponent } from './pages/department-edit/department-edit.component';
import { DepartmentListComponent } from './pages/department-list/department-list.component';

export const DEPARTMENT_ROUTES: Routes = [
{
  path: '',
  canMatch: [roleAtLeast('SUPERVISOR')],
  children: [
    { path: '', component: DepartmentListComponent },
    { path: 'create', component: DepartmentCreateComponent },
    { path: ':id', component: DepartmentDetailsComponent },
    { path: ':id/edit', component: DepartmentEditComponent }
  ]
}
];