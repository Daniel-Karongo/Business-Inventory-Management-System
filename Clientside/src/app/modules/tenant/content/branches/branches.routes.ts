import { Routes } from '@angular/router';
import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { BranchCreateComponent } from './pages/branch-create/branch-create.component';
import { BranchDetailsComponent } from './pages/branch-details/branch-details.component';
import { BranchEditComponent } from './pages/branch-edit/branch-edit.component';
import { BranchListComponent } from './pages/branch-list/branch-list.component';

export const BRANCH_ROUTES: Routes = [
  {
    path: '',
    canMatch: [roleAtLeast('MANAGER')],
    children: [
      { path: '', component: BranchListComponent },
      { path: 'create', component: BranchCreateComponent },
      { path: ':id', component: BranchDetailsComponent },
      { path: ':id/edit', component: BranchEditComponent }
    ]
  }
];