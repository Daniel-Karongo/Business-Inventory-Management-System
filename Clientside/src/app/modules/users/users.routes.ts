import { Routes } from '@angular/router';
import { UserListComponent } from './pages/user-list/user-list.component';
import { UserCreateComponent } from './pages/user-create/user-create.component';
import { UserEditComponent } from './pages/user-edit/user-edit.component';
import { UserDetailsComponent } from './pages/user-details/user-details.component';
import { authGuard } from '../auth/guards/auth.guard';

export const USER_ROUTES: Routes = [
  {
    path: '',
    canActivate: [authGuard],
    children: [
      { path: '', component: UserListComponent },
      { path: 'create', component: UserCreateComponent },
      { path: ':username/edit', component: UserEditComponent },
      { path: ':username', component: UserDetailsComponent }
    ]
  }
];