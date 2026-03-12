import { Routes } from '@angular/router';
import { roleAtLeast } from '../../../../core/security/role-at-least.guard';
import { authGuard } from '../../../auth/guards/auth.guard';
import { UserCreateComponent } from './pages/user-create/user-create.component';
import { UserDetailsComponent } from './pages/user-details/user-details.component';
import { UserEditComponent } from './pages/user-edit/user-edit.component';
import { UserListComponent } from './pages/user-list/user-list.component';

export const USER_ROUTES: Routes = [
  {
    path: '',
    canActivate: [authGuard],
    canMatch: [roleAtLeast('SUPERVISOR')],
    children: [
      { path: '', component: UserListComponent },
      { path: 'create', component: UserCreateComponent },
      { path: ':username/edit', component: UserEditComponent },
      { path: ':username', component: UserDetailsComponent }
    ]
  }
];