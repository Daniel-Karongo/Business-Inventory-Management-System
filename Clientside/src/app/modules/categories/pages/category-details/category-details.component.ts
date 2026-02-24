import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Subject, takeUntil } from 'rxjs';

import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { CategoryService } from '../../services/category.service';
import { Category } from '../../models/category.model';

@Component({
  standalone: true,
  selector: 'app-category-details',
  imports: [
    CommonModule,
    PageShellComponent,
    MatButtonModule,
    MatChipsModule,
    MatProgressSpinnerModule,
    MatTooltipModule
  ],
  templateUrl: './category-details.component.html',
  styleUrls: ['./category-details.component.scss']
})
export class CategoryDetailsComponent implements OnInit, OnDestroy {

  private destroy$ = new Subject<void>();

  category: Category | null = null;
  loading = true;
  error: string | null = null;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private service: CategoryService
  ) {}

  ngOnInit(): void {
    const id = Number(this.route.snapshot.paramMap.get('id'));

    if (!id) {
      this.error = 'Invalid category ID';
      this.loading = false;
      return;
    }

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

  /* =========================================================
     DERIVED PROPERTIES (SAFE)
  ========================================================= */

  get hasSuppliers(): boolean {
    return !!this.category?.suppliers?.length;
  }

  get hasSubcategories(): boolean {
    return !!this.category?.subcategories?.length;
  }

  /* =========================================================
     ACTIONS
  ========================================================= */

  edit(): void {
    if (!this.category) return;
    this.router.navigate(['edit'], { relativeTo: this.route });
  }

  back(): void {
    this.router.navigate(['/categories']);
  }
}