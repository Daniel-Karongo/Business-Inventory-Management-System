import { Routes } from '@angular/router';

export const STOCK_ONBOARDING_ROUTES: Routes = [
  {
    path: '',
    loadComponent: () =>
      import(
        './pages/onboarding-create-page/onboarding-create-page.component'
      ).then(m => m.OnboardingCreatePageComponent)
  }
];