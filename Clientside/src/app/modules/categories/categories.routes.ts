import { Routes } from '@angular/router';
import { authGuard } from '../auth/guards/auth.guard';

export const CATEGORY_ROUTES: Routes = [
  {
    path: '',
    canMatch: [authGuard],
    children: [
      {
        path: '',
        loadComponent: () =>
          import('./pages/category-list/category-list.component')
            .then(m => m.CategoryListComponent)
      },
      {
        path: 'create',
        loadComponent: () =>
          import('./pages/category-create/category-create.component')
            .then(m => m.CategoryCreateComponent)
      },
      {
        path: ':id/edit',
        loadComponent: () =>
          import('./pages/category-edit/category-edit.component')
            .then(m => m.CategoryEditComponent)
      },
      {
        path: ':id',
        loadComponent: () =>
          import('./pages/category-details/category-details.component')
            .then(m => m.CategoryDetailsComponent)
      }
    ]
  }
];