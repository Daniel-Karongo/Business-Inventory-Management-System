import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { Router, RouterModule } from '@angular/router';
import {
  BehaviorSubject,
  combineLatest,
  debounceTime,
  distinctUntilChanged,
  switchMap,
  tap
} from 'rxjs';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatSnackBar } from '@angular/material/snack-bar';

import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { CategoryService } from '../../services/category.service';
import { Category, CategoryFlat } from '../../models/category.model';
import { AuthService } from '../../../auth/services/auth.service';
import { CategoryTreeComponent } from '../../components/category-tree/category-tree.component';

@Component({
  selector: 'app-category-list',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    PageShellComponent,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatTableModule,
    MatTooltipModule,
    CategoryTreeComponent,
    MatPaginatorModule
  ],
  templateUrl: './category-list.component.html',
  styleUrls: ['./category-list.component.scss']
})
export class CategoryListComponent implements OnInit {

  viewMode: 'tree' | 'flat' = 'tree';
  density: 'compact' | 'comfortable' = 'comfortable';

  categories: Category[] = [];
  flatCategories: CategoryFlat[] = [];

  loading = true;

  search$ = new BehaviorSubject<string>('');
  status$ = new BehaviorSubject<'all' | 'active' | 'deleted'>('all');

  selectedIds = new Set<number>();
  expandedIds = new Set<number>();

  displayedColumns = ['select', 'name', 'parent', 'suppliers', 'status', 'actions'];

  canManageDeleted = false;

  pageIndex = 0;
  pageSize = 10;
  pagedData: CategoryFlat[] = [];

  sortField: keyof CategoryFlat | null = null;
  sortDirection: 'asc' | 'desc' = 'asc';

  private readonly STORAGE_KEY = 'category_list_prefs';

  constructor(
    private categoryService: CategoryService,
    private authService: AuthService,
    private snackbar: MatSnackBar,
    private router: Router
  ) { }

  ngOnInit(): void {

    this.loadPreferences();

    const user = this.authService.getSnapshot();
    if (user) {
      this.canManageDeleted =
        ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(user.role);
    }

    combineLatest([
      this.search$.pipe(debounceTime(400), distinctUntilChanged()),
      this.status$
    ])
      .pipe(
        tap(() => this.loading = true),
        switchMap(([search, status]) => {

          const deletedParam =
            status === 'deleted'
              ? true
              : status === 'active'
                ? false
                : null;

          if (search?.trim()) {
            return this.categoryService.search(search.trim(), deletedParam);
          }

          return this.categoryService.getAll('tree', deletedParam);
        }),
        tap(data => {

          // Reset state on reload
          this.selectedIds.clear();

          this.categories = data;
          this.flatCategories = this.flattenTree(data);

          this.pageIndex = 0;
          this.applyPagination();

          this.loading = false;
        })
      )
      .subscribe();
  }

  /* =========================================================
     TREE → FLAT
  ========================================================= */

  private flattenTree(nodes: Category[]): CategoryFlat[] {

    const result: CategoryFlat[] = [];

    const traverse = (cats: Category[], parentName: string | null = null) => {
      for (const c of cats) {

        result.push({
          ...c,
          parentName
        });

        if (c.subcategories?.length) {
          traverse(c.subcategories, c.name);
        }
      }
    };

    traverse(nodes);

    return result;
  }

  /* =========================================================
     PAGINATION
  ========================================================= */

  applyPagination() {
    const start = this.pageIndex * this.pageSize;
    const end = start + this.pageSize;
    this.pagedData = this.flatCategories.slice(start, end);
  }

  changePage(event: PageEvent) {
    this.pageIndex = event.pageIndex;
    this.pageSize = event.pageSize;
    this.applyPagination();
  }

  /* =========================================================
     SORTING (ENTERPRISE)
  ========================================================= */

  sort(field: keyof CategoryFlat) {

    if (this.sortField === field) {
      this.sortDirection =
        this.sortDirection === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDirection = 'asc';
    }

    this.flatCategories.sort((a, b) => {

      let valueA: any = a[field];
      let valueB: any = b[field];

      if (field === 'suppliers') {
        valueA = a.suppliers?.length ?? 0;
        valueB = b.suppliers?.length ?? 0;
      }

      if (field === 'deleted') {
        valueA = a.deleted ? 1 : 0;
        valueB = b.deleted ? 1 : 0;
      }

      valueA = (valueA ?? '').toString().toLowerCase();
      valueB = (valueB ?? '').toString().toLowerCase();

      if (valueA < valueB) return this.sortDirection === 'asc' ? -1 : 1;
      if (valueA > valueB) return this.sortDirection === 'asc' ? 1 : -1;
      return 0;
    });

    this.applyPagination();
  }

  getSortIcon(field: keyof CategoryFlat): string {
    if (this.sortField !== field) return 'unfold_more';
    return this.sortDirection === 'asc'
      ? 'arrow_upward'
      : 'arrow_downward';
  }

  /* =========================================================
     SELECTION
  ========================================================= */

  get hasSelection(): boolean {
    return Array.from(this.selectedIds).length > 0;
  }
  
  toggleSelection(id: number) {

    const findAndApply = (cats: Category[], chain: number[] = []) => {

      for (const c of cats) {

        const currentChain = [...chain, c.id];

        if (c.id === id) {
          this.applySelection(c);

          // Auto expand path for selected nodes
          currentChain.forEach(i => this.expandedIds.add(i));
          return true;
        }

        if (c.subcategories?.length) {
          const found = findAndApply(c.subcategories, currentChain);
          if (found) return true;
        }
      }

      return false;
    };

    findAndApply(this.categories);
    this.selectedIds = new Set(this.selectedIds);
    this.expandedIds = new Set(this.expandedIds);
  }

  private applySelection(category: Category) {

    const isSelected = this.selectedIds.has(category.id);

    const toggleDown = (cat: Category) => {

      if (isSelected) {
        this.selectedIds.delete(cat.id);
      } else {
        this.selectedIds.add(cat.id);
      }

      cat.subcategories?.forEach(toggleDown);
    };

    toggleDown(category);
  }

  toggleRowSelection(row: CategoryFlat) {
    if (this.selectedIds.has(row.id)) {
      this.selectedIds.delete(row.id);
    } else {
      this.selectedIds.add(row.id);
    }

    this.selectedIds = new Set(this.selectedIds);
  }

  clearSelection() {
    this.selectedIds.clear();
  }

  /* =========================================================
     BULK
  ========================================================= */

  bulkDelete() {
    const ids = Array.from(this.selectedIds);
    if (!ids.length) return;

    if (!confirm(`Delete ${ids.length} categories recursively?`)) return;

    this.categoryService.bulkSoftDelete(ids).subscribe(() => {
      this.snackbar.open('Categories deleted', 'Close', { duration: 3000 });
      this.clearSelection();
      this.reload();
    });
  }

  bulkRestore() {
    const ids = Array.from(this.selectedIds);
    if (!ids.length) return;

    this.categoryService.bulkRestore(ids).subscribe(() => {
      this.snackbar.open('Categories restored', 'Close', { duration: 3000 });
      this.clearSelection();
      this.reload();
    });
  }

  /* =========================================================
     VIEW / DENSITY
  ========================================================= */

  setView(mode: 'tree' | 'flat') {
    this.viewMode = mode;
    this.savePreferences();
  }

  toggleDensity() {
    this.density =
      this.density === 'compact'
        ? 'comfortable'
        : 'compact';

    this.savePreferences();
  }

  private savePreferences() {
    localStorage.setItem(this.STORAGE_KEY, JSON.stringify({
      viewMode: this.viewMode,
      density: this.density
    }));
  }

  private loadPreferences() {
    const raw = localStorage.getItem(this.STORAGE_KEY);
    if (!raw) return;

    const prefs = JSON.parse(raw);
    this.viewMode = prefs.viewMode ?? 'tree';
    this.density = prefs.density ?? 'comfortable';
  }

  reload() {
    this.search$.next(this.search$.value);
  }

  onSearch(value: string) {
    this.search$.next(value);
  }

  onStatusChange(value: 'all' | 'active' | 'deleted') {
    this.status$.next(value);
  }
}