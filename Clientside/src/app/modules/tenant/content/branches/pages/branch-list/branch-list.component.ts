import {
  Component,
  OnInit,
  ViewChild
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  ActivatedRoute,
  Router,
  RouterModule
} from '@angular/router';

import { FormsModule } from '@angular/forms';

import { BreakpointObserver } from '@angular/cdk/layout';

import {
  debounceTime,
  distinctUntilChanged,
  Subject
} from 'rxjs';

import {
  filter
} from 'rxjs/operators';

import { MatTableModule } from '@angular/material/table';

import {
  MatPaginator,
  MatPaginatorModule,
  PageEvent
} from '@angular/material/paginator';

import { MatIconModule } from '@angular/material/icon';

import { MatButtonModule } from '@angular/material/button';

import { MatTooltipModule } from '@angular/material/tooltip';

import { MatInputModule } from '@angular/material/input';

import { MatSlideToggleModule } from '@angular/material/slide-toggle';

import { MatCheckboxModule } from '@angular/material/checkbox';

import { MatDialog } from '@angular/material/dialog';

import {
  BranchListItemDTO,
  BranchTableState
} from '../../models/branch.model';

import { PageWrapper } from '../../../../../../core/models/page-wrapper.model';

import { BranchService } from '../../services/branch.service';

import { ConfirmDialogComponent } from '../../../../../../shared/components/confirm-dialog/confirm-dialog.component';

import { BranchBulkImportDialogComponent } from '../../components/branch-bulk-import-dialog/branch-bulk-import-dialog.component';

import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';

@Component({
  standalone: true,
  selector: 'app-branch-list',
  imports: [
    CommonModule,
    RouterModule,
    FormsModule,

    PageShellComponent,

    MatTableModule,
    MatPaginatorModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule,
    MatInputModule,
    MatSlideToggleModule,
    MatCheckboxModule
  ],
  templateUrl: './branch-list.component.html',
  styleUrls: ['./branch-list.component.scss']
})
export class BranchListComponent implements OnInit {

  /* =========================================================
     TABLE
  ========================================================= */

  displayedColumns = [
    'select',
    'code',
    'name',
    'location',
    'security',
    'attendance',
    'status',
    'createdAt',
    'actions'
  ];

  /* =========================================================
     DATA
  ========================================================= */

  branches: BranchListItemDTO[] = [];

  page?: PageWrapper<BranchListItemDTO>;

  loading = false;

  skeletons = Array.from({ length: 12 });

  selectedIds = new Set<string>();

  /* =========================================================
     UI
  ========================================================= */

  isMobile = false;

  viewMode: 'table' | 'grid' = 'table';

  density: 'compact' | 'comfortable' = 'compact';

  private readonly STORAGE_KEY = 'branch_list_preferences';

  @ViewChild(MatPaginator)
  paginator?: MatPaginator;

  /* =========================================================
     TABLE STATE
  ========================================================= */

  state: BranchTableState = {
    search: '',
    includeDeleted: false,
    sortBy: 'createdAt',
    sortDirection: 'desc',
    page: 0,
    size: 20
  };

  /* =========================================================
     SEARCH
  ========================================================= */

  private search$ = new Subject<string>();

  constructor(
    private branchService: BranchService,
    private dialog: MatDialog,
    private route: ActivatedRoute,
    private router: Router,
    private breakpoint: BreakpointObserver
  ) { }

  /* =========================================================
     INIT
  ========================================================= */

  ngOnInit(): void {

    this.loadPreferences();

    this.breakpoint
      .observe(['(max-width: 768px)'])
      .subscribe(res => {

        this.isMobile = res.matches;

        if (this.isMobile) {
          this.viewMode = 'grid';
        }
      });

    this.initializeSearch();

    this.route.queryParamMap
      .pipe(
        filter(() => !this.loading)
      )
      .subscribe(params => {

        this.state = {
          search:
            params.get('search') ?? '',

          includeDeleted:
            params.get('deleted') === 'true',

          sortBy:
            params.get('sortBy') ?? 'createdAt',

          sortDirection:
            (params.get('sortDirection') as 'asc' | 'desc')
            ?? 'desc',

          page:
            Number(params.get('page') ?? 0),

          size:
            Number(params.get('size') ?? 20)
        };

        this.load();
      });
  }

  /* =========================================================
     LOAD
  ========================================================= */

  load(): void {

    this.loading = true;

    this.branchService
      .getAll(this.state)
      .subscribe({
        next: (page) => {

          this.page = page;

          this.branches = page.content ?? [];

          this.loading = false;
        },

        error: () => {
          this.loading = false;
        }
      });
  }

  /* =========================================================
     ROUTE STATE
  ========================================================= */

  private updateRoute(): void {

    this.router.navigate(
      [],
      {
        relativeTo: this.route,

        queryParams: {
          search:
            this.state.search || null,

          deleted:
            this.state.includeDeleted || null,

          sortBy:
            this.state.sortBy,

          sortDirection:
            this.state.sortDirection,

          page:
            this.state.page,

          size:
            this.state.size
        },

        queryParamsHandling: 'merge'
      }
    );
  }

  /* =========================================================
     SEARCH
  ========================================================= */

  private initializeSearch(): void {

    this.search$
      .pipe(
        debounceTime(350),
        distinctUntilChanged()
      )
      .subscribe(search => {

        this.state.search = search;

        this.state.page = 0;

        this.updateRoute();
      });
  }

  onSearchChange(value: string): void {
    this.search$.next(value);
  }

  /* =========================================================
     FILTERS
  ========================================================= */

  toggleDeleted(value: boolean): void {

    this.state.includeDeleted = value;

    this.state.page = 0;

    this.updateRoute();
  }

  /* =========================================================
     PAGINATION
  ========================================================= */

  changePage(event: PageEvent): void {

    this.state.page = event.pageIndex;

    this.state.size = event.pageSize;

    this.updateRoute();
  }

  /* =========================================================
     SORT
  ========================================================= */

  sort(sortBy: string): void {

    if (this.state.sortBy === sortBy) {

      this.state.sortDirection =
        this.state.sortDirection === 'asc'
          ? 'desc'
          : 'asc';

    } else {

      this.state.sortBy = sortBy;

      this.state.sortDirection = 'asc';
    }

    this.state.page = 0;

    this.updateRoute();
  }

  getSortIcon(field: string): string {

    if (this.state.sortBy !== field) {
      return 'unfold_more';
    }

    return this.state.sortDirection === 'asc'
      ? 'arrow_upward'
      : 'arrow_downward';
  }

  /* =========================================================
     VIEW MODES
  ========================================================= */

  setViewMode(mode: 'table' | 'grid'): void {

    this.viewMode = mode;

    this.savePreferences();
  }

  toggleDensity(): void {

    this.density =
      this.density === 'compact'
        ? 'comfortable'
        : 'compact';

    this.savePreferences();
  }

  /* =========================================================
     SELECTION
  ========================================================= */

  toggleSelection(branch: BranchListItemDTO): void {

    if (this.selectedIds.has(branch.id)) {
      this.selectedIds.delete(branch.id);
    } else {
      this.selectedIds.add(branch.id);
    }
  }

  isSelected(id: string): boolean {
    return this.selectedIds.has(id);
  }

  isAllSelected(): boolean {

    return this.branches.length > 0 &&
      this.branches.every(b => this.selectedIds.has(b.id));
  }

  masterToggle(): void {

    if (this.isAllSelected()) {

      this.branches.forEach(b =>
        this.selectedIds.delete(b.id)
      );

    } else {

      this.branches.forEach(b =>
        this.selectedIds.add(b.id)
      );
    }
  }

  clearSelection(): void {
    this.selectedIds.clear();
  }

  get bulkState(): 'none' | 'active' | 'deleted' | 'mixed' {

    if (this.selectedIds.size === 0) {
      return 'none';
    }

    const selected =
      this.branches.filter(
        b => this.selectedIds.has(b.id)
      );

    const deletedCount =
      selected.filter(b => b.deleted).length;

    if (deletedCount === 0) {
      return 'active';
    }

    if (deletedCount === selected.length) {
      return 'deleted';
    }

    return 'mixed';
  }

  private getSelectedBranches(): BranchListItemDTO[] {

    return this.branches.filter(
      b => this.selectedIds.has(b.id)
    );
  }

  bulkDelete(): void {

    const ids =
      this.getSelectedBranches()
        .map(b => b.id);

    if (ids.length === 0) {
      return;
    }

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        data: {
          title: 'Delete Branches',
          message: `Delete ${ids.length} branches?`
        }
      }
    );

    ref.afterClosed()
      .subscribe(ok => {

        if (!ok) {
          return;
        }

        this.branchService
          .deleteBulk(ids, true)
          .subscribe(() => {

            this.clearSelection();

            this.load();
          });
      });
  }

  bulkRestore(): void {

    const ids =
      this.getSelectedBranches()
        .map(b => b.id);

    if (ids.length === 0) {
      return;
    }

    this.branchService
      .restoreBulk(ids)
      .subscribe(() => {

        this.clearSelection();

        this.load();
      });
  }

  bulkHardDelete(): void {

    const ids =
      this.getSelectedBranches()
        .map(b => b.id);

    if (ids.length === 0) {
      return;
    }

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        data: {
          title: 'Permanently Delete Branches',
          message:
            `Permanently delete ${ids.length} branches? This cannot be undone.`
        }
      }
    );

    ref.afterClosed()
      .subscribe(ok => {

        if (!ok) {
          return;
        }

        this.branchService
          .deleteBulk(ids, false)
          .subscribe(() => {

            this.clearSelection();

            this.load();
          });
      });
  }

  /* =========================================================
     BULK IMPORT
  ========================================================= */

  openBulkImport(): void {

    this.dialog.open(
      BranchBulkImportDialogComponent,
      {
        width: '1100px',
        maxWidth: '95vw',
        maxHeight: '90vh',
        autoFocus: false
      }
    )
      .afterClosed()
      .subscribe(imported => {

        if (imported === true) {
          this.load();
        }
      });
  }

  /* =========================================================
     DELETE
  ========================================================= */

  delete(branch: BranchListItemDTO): void {

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        data: {
          title: 'Delete Branch',
          message: `Delete ${branch.name}?`
        }
      }
    );

    ref.afterClosed()
      .subscribe(ok => {

        if (!ok || !branch.id) {
          return;
        }

        this.branchService
          .delete(branch.id, true)
          .subscribe(() => {

            this.clearSelection();

            this.load();
          });
      });
  }

  /* =========================================================
     RESTORE
  ========================================================= */

  restore(
    branch: BranchListItemDTO
  ): void {

    this.branchService
      .restore(branch.id)
      .subscribe(() => {

        this.selectedIds.delete(
          branch.id
        );

        this.load();
      });
  }

  /* =========================================================
     HARD DELETE
  ========================================================= */

  hardDelete(
    branch: BranchListItemDTO
  ): void {

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        data: {
          title: 'Permanent Delete Branch',
          message:
            `Permanently delete ${branch.name}? This action cannot be undone.`
        }
      }
    );

    ref.afterClosed()
      .subscribe(ok => {

        if (!ok) {
          return;
        }

        this.branchService
          .delete(
            branch.id,
            false
          )
          .subscribe(() => {

            this.clearSelection();

            this.load();
          });
      });
  }

  /* =========================================================
     HELPERS
  ========================================================= */

  trackById(
    _: number,
    item: BranchListItemDTO
  ): string {

    return item.id;
  }

  /* =========================================================
     PREFS
  ========================================================= */

  savePreferences(): void {

    localStorage.setItem(
      this.STORAGE_KEY,
      JSON.stringify({
        viewMode: this.viewMode,
        density: this.density
      })
    );
  }

  private loadPreferences(): void {

    const raw =
      localStorage.getItem(this.STORAGE_KEY);

    if (!raw) {
      return;
    }

    try {

      const prefs = JSON.parse(raw);

      this.viewMode =
        prefs.viewMode ?? this.viewMode;

      this.density =
        prefs.density ?? this.density;

    } catch {
      // ignore
    }
  }
}