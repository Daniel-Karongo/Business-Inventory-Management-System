import { Routes } from '@angular/router';
import { DepartmentListComponent } from './pages/department-list/department-list.component';
import { DepartmentCreateComponent } from './pages/department-create/department-create.component';
import { DepartmentEditComponent } from './pages/department-edit/department-edit.component';
import { DepartmentDetailsComponent } from './pages/department-details/department-details.component';

export const DEPARTMENT_ROUTES: Routes = [
  { path: '', component: DepartmentListComponent },
  { path: 'create', component: DepartmentCreateComponent },
  { path: ':id', component: DepartmentDetailsComponent },
  { path: ':id/edit', component: DepartmentEditComponent }
];