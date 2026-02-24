import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { Subject, takeUntil } from 'rxjs';
import { MatSnackBar } from '@angular/material/snack-bar';

import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { CategoryFormComponent } from '../../components/category-form/category-form.component';
import { CategoryService } from '../../services/category.service';
import { Category } from '../../models/category.model';

@Component({
  standalone: true,
  selector: 'app-category-edit',
  imports: [
    CommonModule,
    PageShellComponent,
    CategoryFormComponent
  ],
  templateUrl: './category-edit.component.html'
})
export class CategoryEditComponent implements OnInit, OnDestroy {

  private destroy$ = new Subject<void>();

  category: Category | null = null;
  loading = true;
  error: string | null = null;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private service: CategoryService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {

    const id = Number(this.route.snapshot.paramMap.get('id'));

    this.service.getById(id, 'tree', false)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (res) => {
          this.category = res;
          this.loading = false;
        },
        error: () => {
          this.error = 'Failed to load category.';
          this.loading = false;
        }
      });
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  handleSubmit(event: { payload: any, done: () => void }): void {

    if (!this.category) return;

    this.service.update(this.category.id, event.payload).subscribe({
      next: () => {
        event.done();
        this.snackbar.open('Category updated', 'Close', { duration: 3000 });
        this.router.navigate(['/categories', this.category!.id]);
      },
      error: (err) => {
        event.done();

        const message =
          err?.error?.message ||
          'Failed to update category';

        this.snackbar.open(message, 'Close', { duration: 4000 });
      }
    });
  }
}