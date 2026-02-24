import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { MatSnackBar } from '@angular/material/snack-bar';

import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { CategoryFormComponent } from '../../components/category-form/category-form.component';
import { CategoryService } from '../../services/category.service';

@Component({
  standalone: true,
  selector: 'app-category-create',
  imports: [PageShellComponent, CategoryFormComponent],
  templateUrl: './category-create.component.html'
})
export class CategoryCreateComponent {

  constructor(
    private service: CategoryService,
    private router: Router,
    private snackbar: MatSnackBar
  ) { }

  handleSubmit(event: { payload: any, done: () => void }): void {

    this.service.create(event.payload).subscribe({
      next: (created) => {
        event.done();
        this.snackbar.open('Category created', 'Close', { duration: 3000 });
        this.router.navigate(['/categories', created.id]);
      },
      error: () => {
        event.done();
        this.snackbar.open('Failed to create category', 'Close', { duration: 3000 });
      }
    });
  }
}