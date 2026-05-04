import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Subject, takeUntil } from 'rxjs';

import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { CategoryService } from '../../services/category.service';
import { Category } from '../../models/category.model';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { CategoryTreeComponent } from '../../components/category-tree/category-tree.component';
import { ConfirmDialogComponent } from '../../../../../../shared/components/confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  standalone: true,
  selector: 'app-category-details',
  imports: [
    CommonModule,
    PageShellComponent,
    MatButtonModule,
    MatChipsModule,
    MatProgressSpinnerModule,
    MatTooltipModule,
    MatIconModule,
    CategoryTreeComponent
  ],
  templateUrl: './category-details.component.html',
  styleUrls: ['./category-details.component.scss']
})
export class CategoryDetailsComponent implements OnInit, OnDestroy {

  private destroy$ = new Subject<void>();

  category: Category | null = null;
  loading = true;
  error: string | null = null;
  expandedInline = new Set<number>();
  readonly emptySelection = new Set<number>();
  breadcrumb: Category[] = [];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private service: CategoryService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {

    this.route.paramMap
      .pipe(takeUntil(this.destroy$))
      .subscribe(params => {

        const idParam = params.get('id');
        const id = idParam ? Number(idParam) : null;

        if (!id || isNaN(id)) {
          this.error = 'Invalid category ID';
          this.loading = false;
          return;
        }

        this.fetchCategory(id);
      });
  }

  private fetchCategory(id: number): void {
    this.loading = true;
    this.error = null;

    this.service.getById(id, 'tree', null)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (res) => {
          this.category = res;
          this.expandedInline = new Set([res.id]);

          this.service.getAncestors(id)
            .subscribe({
              next: (path) => {
                this.breadcrumb = path;
                this.loading = false;
              },
              error: () => {
                this.error = 'Failed to load breadcrumb';
                this.loading = false;
              }
            });
        },
        error: () => {
          this.error = 'Failed to load category';
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

  get supplierCount(): number {
    return this.category?.suppliers?.length || 0;
  }

  get subcategoryCount(): number {
    return this.category?.subcategories?.length || 0;
  }

  get hasSubcategories(): boolean {
    return !!this.category?.subcategories?.length;
  }

  trackById(_: number, item: any) {
    return item?.id;
  }



  toggleInlineExpand(id: number) {
    if (this.expandedInline.has(id)) {
      this.expandedInline.delete(id);
    } else {
      this.expandedInline.add(id);
    }
    this.expandedInline = new Set(this.expandedInline);
  }

  /* =========================================================
     ACTIONS
  ========================================================= */

  openSubcategory(id: number): void {
    this.router.navigate(['/app/categories', id]);
  }

  openSupplier(id: string): void {
    this.router.navigate(['/app/suppliers', id]);
  }

  toggleStatus(): void {
    if (!this.category) return;

    const isDeleted = this.category.deleted;

    const dialogRef = this.snackbar.open(
      isDeleted ? 'Restoring category...' : 'Disabling category...',
      'Close',
      { duration: 1500 }
    );

    const action$ = isDeleted
      ? this.service.restoreRecursive(this.category.id)
      : this.service.softDelete(this.category.id);

    action$.subscribe({
      next: () => {
        const newDeleted = !isDeleted;

        this.category = this.applyDeletedState(this.category!, newDeleted);
        this.category = { ...this.category! };
        
        this.expandedInline = new Set(this.expandedInline);
        this.snackbar.open(
          newDeleted ? 'Category disabled' : 'Category restored',
          'Close',
          { duration: 3000 }
        );
      },
      error: () => {
        this.snackbar.open('Action failed', 'Close', { duration: 3000 });
      }
    });
  }

  restoreSingle(): void {
    if (!this.category) return;

    this.service.restore(this.category.id).subscribe({
      next: () => {
        this.category = {
          ...this.category!,
          deleted: false
        };

        this.snackbar.open('Category restored (single)', 'Close', { duration: 3000 });
      },
      error: () => {
        this.snackbar.open('Restore failed', 'Close', { duration: 3000 });
      }
    });
  }

  private applyDeletedState(node: Category, deleted: boolean): Category {
    return {
      ...node,
      deleted,
      subcategories: node.subcategories
        ? node.subcategories.map(child =>
          this.applyDeletedState(child, deleted)
        )
        : []
    };
  }

  hardDelete(): void {
    if (!this.category) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Permanent Delete',
        message: `Delete "${this.category.name}" permanently? This cannot be undone.`,
        confirmText: 'Delete Permanently',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(confirmed => {
      if (!confirmed) return;

      this.service.hardDelete(this.category!.id)
        .subscribe({
          next: () => {
            this.snackbar.open('Category permanently deleted', 'Close', { duration: 3000 });
            this.back();
          },
          error: () => {
            this.snackbar.open('Hard delete failed', 'Close', { duration: 3000 });
          }
        });
    });
  }

  private updateSubtreeDeletedState(
    nodes: Category[],
    deleted: boolean
  ): Category[] {
    return nodes.map(node => ({
      ...node,
      deleted,
      subcategories: node.subcategories
        ? this.updateSubtreeDeletedState(node.subcategories, deleted)
        : []
    }));
  }

  edit(): void {
    if (!this.category) return;
    this.router.navigate(['edit'], { relativeTo: this.route });
  }

  back(): void {
    this.router.navigate(['/app/categories']);
  }
}