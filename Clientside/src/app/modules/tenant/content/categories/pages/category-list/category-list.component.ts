import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { Router, RouterModule } from '@angular/router';
import {
  BehaviorSubject,
  combineLatest,
  debounceTime,
  distinctUntilChanged,
  finalize,
  of,
  switchMap,
  tap
} from 'rxjs';

import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';

import { MatDialog } from '@angular/material/dialog';
import { BranchContextService } from '../../../../../../core/services/branch-context.service';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { AuthService } from '../../../../../auth/services/auth.service';
import { CategoryBulkImportDialogComponent } from '../../components/category-bulk-import-dialog/category-bulk-import-dialog.component';
import { CategoryTreeComponent } from '../../components/category-tree/category-tree.component';
import { Category, CategoryFlat } from '../../models/category.model';
import { CategoryService } from '../../services/category.service';
import { BranchDTO } from '../../../branches/models/branch.model';
import { BranchService } from '../../../branches/services/branch.service';
import { ConfirmDialogComponent } from '../../../../../../shared/components/confirm-dialog/confirm-dialog.component';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

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
    MatPaginatorModule,
    MatProgressSpinnerModule
  ],
  templateUrl: './category-list.component.html',
  styleUrls: ['./category-list.component.scss']
})
export class CategoryListComponent implements OnInit {

  pageBranch$ = new BehaviorSubject<string | null>(null);

  branches: BranchDTO[] = [];
  canSelectBranch = false;

  viewMode: 'tree' | 'flat' = 'tree';
  density: 'compact' | 'comfortable' = 'comfortable';

  categories: Category[] = [];
  flatCategories: CategoryFlat[] = [];

  loading = true;
  rowLoading = new Set<number>();

  search$ = new BehaviorSubject<string>('');
  status$ = new BehaviorSubject<'all' | 'active' | 'deleted'>('all');
  private refresh$ = new BehaviorSubject<void>(undefined);

  selectedIds = new Set<number>();
  expandedIds = new Set<number>();

  displayedColumns = ['select', 'name', 'parent', 'suppliers', 'status', 'actions'];

  pageIndex = 0;
  pageSize = 20;
  pagedData: CategoryFlat[] = [];

  canManageDeleted = false;
  sortField: keyof CategoryFlat | null = null;
  sortDirection: 'asc' | 'desc' = 'asc';

  private readonly STORAGE_KEY = 'category_list_prefs';

  constructor(
    private categoryService: CategoryService,
    public branchContext: BranchContextService,
    private authService: AuthService,
    private branchService: BranchService,
    private snackbar: MatSnackBar,
    private router: Router,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.loadPreferences();

    const user = this.authService.getSnapshot();
    if (!user) return;

    this.canManageDeleted =
      ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(user.role);

    this.branchService.getAll(false).subscribe(branches => {
      this.branches = branches;

      // ✅ RULE: only allow selector if MORE THAN ONE branch
      this.canSelectBranch = branches.length > 1;

      // ✅ AUTO-SET when only one branch
      if (branches.length === 1) {
        const id = branches[0]?.id ?? null;
        this.pageBranch$.next(id);
      }
    });

    this.initDataStream();
  }

  private initDataStream() {

    combineLatest([
      this.search$.pipe(debounceTime(400), distinctUntilChanged()),
      this.status$,
      this.branchContext.branch$,
      this.pageBranch$,
      this.refresh$
    ])
      .pipe(
        tap(() => this.loading = true),
        switchMap(([search, status, globalBranch, localBranch]) => {

          const branchId = localBranch ?? globalBranch;

          if (!branchId) {
            this.loading = false;
            return of([]);
          }

          const deletedParam =
            status === 'deleted'
              ? true
              : status === 'active'
                ? false
                : null;

          if (search?.trim()) {
            return this.categoryService.search(
              search.trim(),
              deletedParam,
              branchId // 🔥 IMPORTANT
            );
          }

          return this.categoryService.getAll(
            'tree',
            deletedParam,
            branchId // 🔥 IMPORTANT
          );
        }),
        tap(data => {

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

  private getActiveBranch(): string {
    return this.pageBranch$.value ?? this.branchContext.currentBranch!;
  }

  setPageBranch(branchId: string) {
    this.pageBranch$.next(branchId);
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    if (!this.selectedIds.size) return null;

    const selected = this.flatCategories
      .filter(c => this.selectedIds.has(c.id));

    const states = new Set(selected.map(c => c.deleted));

    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }
  /* =========================================================
     TREE → FLAT
  ========================================================= */

  private flattenTree(nodes: Category[]): CategoryFlat[] {

    const result: CategoryFlat[] = [];

    const traverse = (
      cats: Category[],
      parentId: number | null = null,
      parentName: string | null = null
    ) => {
      for (const c of cats) {

        result.push({
          ...c,
          parentId,     // 🔥 correct source
          parentName
        });

        if (c.subcategories?.length) {
          traverse(c.subcategories, c.id, c.name);
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

    // determine target state
    const targetState = category.deleted;

    // 🔥 prevent mixing
    const existingState = this.bulkState;

    if (!isSelected && existingState && existingState !== 'mixed') {
      if (
        (existingState === 'deleted' && !targetState) ||
        (existingState === 'active' && targetState)
      ) {
        return; // ❌ block selection
      }
    }

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

  openBulkImport() {
    const ref = this.dialog.open(
      CategoryBulkImportDialogComponent,
      {
        width: '1100px',
        maxWidth: '95vw',
        maxHeight: '95vh', // 🔴 CRITICAL
        panelClass: 'bulk-import-dialog'
      }
    );

    ref.afterClosed().subscribe(res => {
      if (res) {
        this.reload();
      }
    });
  }

  bulkDelete() {
    const ids = Array.from(this.selectedIds);
    if (!ids.length) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Delete Categories',
        message: `Are you sure you want to delete ${ids.length} categories? This will apply recursively.`,
        confirmText: 'Delete',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(confirmed => {
      if (!confirmed) return;

      const branchId = this.getActiveBranch();

      this.categoryService.bulkSoftDelete(ids, branchId)
        .subscribe(() => {
          this.snackbar.open('Deleted', 'Close', { duration: 3000 });

          this.clearSelection();

          this.reload(); // ✅ CORRECT
        });
    });
  }

  bulkRestore() {
    const ids = Array.from(this.selectedIds);
    if (!ids.length) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Restore Categories',
        message: `Restore ${ids.length} categories?`,
        confirmText: 'Restore',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(confirmed => {
      if (!confirmed) return;

      const branchId = this.getActiveBranch();

      this.categoryService.bulkRestore(ids, branchId)
        .subscribe(() => {
          this.snackbar.open('Restored', 'Close', { duration: 3000 });
          this.clearSelection();
          this.reload();
        });
    });
  }

  private normalizeToRootIds(ids: number[]): number[] {

    const selected = new Set(ids);

    // map id -> node
    const map = new Map<number, CategoryFlat>();
    this.flatCategories.forEach(c => map.set(c.id, c));

    const roots: number[] = [];

    for (const id of ids) {

      let current = map.get(id);
      let isChild = false;

      while (current?.parentId) {

        if (selected.has(current.parentId)) {
          isChild = true;
          break;
        }

        current = map.get(current.parentId);
      }

      if (!isChild) {
        roots.push(id);
      }
    }

    return roots;
  }

  bulkRestoreRecursive() {
    const rawIds = Array.from(this.selectedIds);
    const ids = this.normalizeToRootIds(rawIds);

    if (!ids.length) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Restore Categories (Recursive)',
        message: `Restore ${ids.length} categories and their subtrees?`,
        confirmText: 'Restore',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;

      const branchId = this.getActiveBranch();

      // backend endpoint exists per controller
      // if not wired, stop here and tell me
      this.categoryService
        .restoreRecursiveBulk(ids, branchId) // ⬅️ if this method is missing, tell me
        .subscribe({
          next: () => {
            ids.forEach(id => this.applyDeletedStateLocal(id, false));
            ids.forEach(id => this.expandPathTo(id));
            this.clearSelection();
            this.snackbar.open('Restored (recursive)', 'Close', { duration: 2500 });
          },
          error: () => this.snackbar.open('Restore failed', 'Close', { duration: 2500 })
        });
    });
  }

  bulkHardDelete() {
    const ids = Array.from(this.selectedIds);
    if (!ids.length) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Permanent Delete',
        message: `Permanently delete ${ids.length} categories? This cannot be undone.`,
        confirmText: 'Delete Permanently',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(confirmed => {
      if (!confirmed) return;

      const branchId = this.getActiveBranch();

      this.categoryService.bulkHardDelete(ids, branchId)
        .subscribe(() => {
          this.snackbar.open('Permanently deleted', 'Close', { duration: 3000 });
          this.clearSelection();
          this.reload();
        });
    });
  }

  // ===== ROW ACTIONS =====

  rowDisable(row: CategoryFlat) {
    if (this.rowLoading.has(row.id)) return; // ✅ guard

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Disable Category',
        message: `Disable "${row.name}" and its subtree?`,
        confirmText: 'Disable',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;

      const branchId = this.getActiveBranch();

      this.rowLoading.add(row.id); // ✅ start loading

      this.categoryService.softDelete(row.id, branchId)
        .pipe(finalize(() => this.rowLoading.delete(row.id))) // ✅ always cleanup
        .subscribe({
          next: () => {
            this.applyDeletedStateLocal(row.id, true);
            this.expandPathTo(row.id);
            this.snackbar.open('Category disabled', 'Close', { duration: 2500 });
          },
          error: () => this.snackbar.open('Action failed', 'Close', { duration: 2500 })
        });
    });
  }

  rowRestoreRecursive(row: CategoryFlat) {
    if (this.rowLoading.has(row.id)) return;

    const node = this.findNode(this.categories, row.id);
    if (!node?.subcategories?.length) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Restore Category (Recursive)',
        message: `Restore "${row.name}" and its subtree?`,
        confirmText: 'Restore',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;

      const branchId = this.getActiveBranch();

      this.rowLoading.add(row.id);

      this.categoryService.restoreRecursive(row.id, branchId)
        .pipe(finalize(() => this.rowLoading.delete(row.id)))
        .subscribe({
          next: () => {
            this.applyDeletedStateLocal(row.id, false);
            this.expandPathTo(row.id);
            this.snackbar.open('Category restored', 'Close', { duration: 2500 });
          },
          error: () => this.snackbar.open('Restore failed', 'Close', { duration: 2500 })
        });
    });
  }

  rowRestoreSingle(row: CategoryFlat) {
    if (this.rowLoading.has(row.id)) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Restore Category',
        message: `Restore only "${row.name}"?`,
        confirmText: 'Restore',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;

      const branchId = this.getActiveBranch();

      this.rowLoading.add(row.id);

      this.categoryService.restore(row.id, branchId)
        .pipe(finalize(() => this.rowLoading.delete(row.id)))
        .subscribe({
          next: () => {
            const update = (nodes: Category[]): Category[] =>
              nodes.map(n => n.id === row.id
                ? { ...n, deleted: false }
                : { ...n, subcategories: n.subcategories ? update(n.subcategories) : [] }
              );

            this.categories = update(this.categories);
            this.flatCategories = this.flattenTree(this.categories);
            this.applyPagination();
            this.expandPathTo(row.id);

            this.snackbar.open('Category restored (single)', 'Close', { duration: 2500 });
          },
          error: () => this.snackbar.open('Restore failed', 'Close', { duration: 2500 })
        });
    });
  }

  rowHardDelete(row: CategoryFlat) {
    if (this.rowLoading.has(row.id)) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      width: '400px',
      data: {
        title: 'Permanent Delete',
        message: `Delete "${row.name}" permanently? This cannot be undone.`,
        confirmText: 'Delete Permanently',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;

      const branchId = this.getActiveBranch();

      this.rowLoading.add(row.id);

      this.categoryService.hardDelete(row.id, branchId)
        .pipe(finalize(() => this.rowLoading.delete(row.id)))
        .subscribe({
          next: () => {
            const remove = (nodes: Category[]): Category[] =>
              nodes
                .filter(n => n.id !== row.id)
                .map(n => ({
                  ...n,
                  subcategories: n.subcategories ? remove(n.subcategories) : []
                }));

            this.categories = remove(this.categories);
            this.flatCategories = this.flattenTree(this.categories);
            this.applyPagination();

            this.snackbar.open('Category permanently deleted', 'Close', { duration: 2500 });
          },
          error: () => this.snackbar.open('Hard delete failed', 'Close', { duration: 2500 })
        });
    });
  }

  // ===== ROW ACTION HELPERS =====

  hasChildren(row: CategoryFlat): boolean {
    const found = this.flatCategories.find(c => c.id === row.id);
    // flatCategories doesn't carry children; derive from original tree
    const node = this.findNode(this.categories, row.id);
    return !!node?.subcategories?.length;
  }

  private findNode(nodes: Category[], id: number): Category | null {
    for (const n of nodes) {
      if (n.id === id) return n;
      if (n.subcategories?.length) {
        const f = this.findNode(n.subcategories, id);
        if (f) return f;
      }
    }
    return null;
  }

  private expandPathTo(id: number) {
    const path: number[] = [];
    const dfs = (nodes: Category[], acc: number[]): boolean => {
      for (const n of nodes) {
        const next = [...acc, n.id];
        if (n.id === id) {
          path.push(...next);
          return true;
        }
        if (n.subcategories?.length && dfs(n.subcategories, next)) return true;
      }
      return false;
    };
    dfs(this.categories, []);
    path.forEach(i => this.expandedIds.add(i));
    this.expandedIds = new Set(this.expandedIds);
  }

  private applyDeletedStateLocal(id: number, deleted: boolean) {

    const update = (nodes: Category[]): Category[] => {
      return nodes.map(n => {

        if (n.id === id) {
          return this.applyDeletedStateTree(n, deleted);
        }

        if (n.subcategories?.length) {
          return {
            ...n,
            subcategories: update(n.subcategories)
          };
        }

        return { ...n };
      });
    };

    this.categories = update(this.categories);

    // 🔥 CRITICAL
    this.categories = [...this.categories];

    this.flatCategories = this.flattenTree(this.categories);
    this.applyPagination();
  }

  private applyDeletedStateTree(node: Category, deleted: boolean): Category {
    return {
      ...node,
      deleted,
      subcategories: node.subcategories?.map(c => this.applyDeletedStateTree(c, deleted)) ?? []
    };
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
    this.refresh$.next();
  }

  onSearch(value: string) {
    this.search$.next(value);
  }

  onStatusChange(value: 'all' | 'active' | 'deleted') {
    this.status$.next(value);
  }
}