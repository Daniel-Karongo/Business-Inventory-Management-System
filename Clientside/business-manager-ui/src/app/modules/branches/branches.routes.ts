import { Routes } from '@angular/router';
import { BranchListComponent } from './pages/branch-list/branch-list.component';
import { BranchCreateComponent } from './pages/branch-create/branch-create.component';
import { BranchEditComponent } from './pages/branch-edit/branch-edit.component';
import { BranchDetailsComponent } from './pages/branch-details/branch-details.component';

export const BRANCH_ROUTES: Routes = [
  { path: '', component: BranchListComponent },
  { path: 'create', component: BranchCreateComponent },
  { path: ':id', component: BranchDetailsComponent },
  { path: ':id/edit', component: BranchEditComponent }
];