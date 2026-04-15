import { Routes } from '@angular/router';
import { LoginComponent } from './pages/login/login.component';

export const AUTH_ROUTES: Routes = [
  {
    path: '',
    component: LoginComponent
  },
  {
    path: 'forgot-password',
    loadComponent: () =>
      import('./pages/forgot-password/forgot-password.component')
        .then(m => m.ForgotPasswordComponent)
  },
  {
    path: 'reset-password',
    loadComponent: () =>
      import('./pages/reset-password/reset-password.component')
        .then(m => m.ResetPasswordComponent)
  }
];