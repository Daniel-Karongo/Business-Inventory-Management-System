import {
  Component,
  OnInit,
  ViewChild
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  RouterModule
} from '@angular/router';

import { FormsModule } from '@angular/forms';

import { BreakpointObserver } from '@angular/cdk/layout';

import {
  debounceTime,
  distinctUntilChanged,
  Subject
} from 'rxjs';


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
  BranchDeletionMode,
  BranchListItemDTO,
  BranchTableState
} from '../../models/branch.model';

import { PageWrapper } from '../../../../../../core/models/page-wrapper.model';

import { BranchService } from '../../services/branch.service';

import { ConfirmDialogComponent } from '../../../../../../shared/components/confirm-dialog/confirm-dialog.component';

import { BranchBulkImportDialogComponent } from '../../components/branch-bulk-import-dialog/branch-bulk-import-dialog.component';

import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
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
    MatCheckboxModule,
    MatSnackBarModule
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

  state: BranchTableState = {
    search: '',
    includeDeleted: false,
    sortBy: 'createdAt',
    sortDirection: 'desc',
    page: 0,
    size: 20
  };

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
     SEARCH
  ========================================================= */

  private search$ = new Subject<string>();

  constructor(
    private branchService: BranchService,
    private dialog: MatDialog,
    private breakpoint: BreakpointObserver,
    private snackBar: MatSnackBar
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

    this.load();
  }

  /* =========================================================
     LOAD
  ========================================================= */

  load(
    showError = true
  ): void {

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

          if (showError) {

            this.showError(
              'Failed to load branches.'
            );
          }
        }
      });
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

        this.load();
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

    this.load();
  }

  /* =========================================================
     PAGINATION
  ========================================================= */

  changePage(event: PageEvent): void {

    this.state.page = event.pageIndex;

    this.state.size = event.pageSize;

    this.load();
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

    this.load();
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
        width: '90vw',
        maxWidth: '1000px',
        panelClass: 'enterprise-dialog',
        autoFocus: false,
        restoreFocus: false,
        data: {
          title: 'Delete Branches',

          severity: 'warning',

          message:
            `Choose how you want to delete ${ids.length} selected branches.`,

          confirmText: 'Delete Branches',

          options: [
            {
              value:
                BranchDeletionMode.SOFT_CONFIGURATION_ONLY,

              label:
                'Configuration Only',

              description:
                'Remove branch configuration while preserving operational history.',

              collapsible: true,
              initiallyExpanded: false,

              deletes: [
                'Products',
                'Product Variants',
                'Categories',
                'Inventory Structure',
                'Suppliers',
                'Customers',
                'Branch Settings',
                'Notification Settings'
              ],

              keeps: [
                'Sales',
                'Purchases',
                'Payments',
                'Accounting',
                'Tax Records',
                'Audit History'
              ]
            },

            {
              value:
                BranchDeletionMode.SOFT_FULL,

              label:
                'Full Soft Delete',

              collapsible: true,
              initiallyExpanded: true,

              description:
                'Remove operational and configuration data while allowing future restoration.',

              deletes: [
                'Configuration Data',
                'Sales',
                'Purchases',
                'Inventory',
                'Accounting',
                'Tax',
                'Messaging',
                'Attendance'
              ],

              keeps: [
                'Branch Records For Future Restoration'
              ]
            }
          ],

          selectedOption:
            BranchDeletionMode.SOFT_FULL,

          requireAcknowledgement: true,

          acknowledgementText:
            'I understand the consequences of this deletion operation.'
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.branchService
          .deleteBulk(
            ids,
            result.option
          )
          .subscribe({
            next: () => {
              this.clearSelection();

              this.showSuccess(
                `${ids.length} branch(es) deleted successfully.`
              );

              this.load(false);
            },

            error: () => {
              this.showError(
                'Failed to delete selected branches.'
              );
            }
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
      .subscribe({
        next: () => {

          this.clearSelection();

          this.showSuccess(
            `${ids.length} branch(es) restored successfully.`
          );

          this.load(false);
        },

        error: () => {
          this.showError(
            'Failed to restore selected branches.'
          );
        }
      });
  }

  /* =========================================================
   BULK HARD DELETE
========================================================= */
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
        width: '750px',
        maxWidth: '95vw',
        panelClass: 'enterprise-dialog',
        autoFocus: false,
        restoreFocus: false,

        data: {
          title:
            'Permanently Delete Branches',

          severity: 'danger',

          message:
            `You are about to permanently delete ${ids.length} soft-deleted branches.`,

          collapsible: true,
          initiallyExpanded: false,

          details: [
            'All remaining branch records will be permanently deleted.',
            'Associated filesystem storage will be deleted.',
            'Deleted branches cannot be restored.',
            'This operation is irreversible.'
          ],

          confirmText:
            'Delete Permanently',

          requireAcknowledgement: true,

          acknowledgementText:
            'I understand these branches will be permanently removed.'
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.branchService
          .deleteBulk(
            ids,
            BranchDeletionMode.HARD
          )
          .subscribe({
            next: () => {

              this.clearSelection();

              this.showSuccess(
                `${ids.length} branch(es) permanently deleted.`
              );

              this.load(false);
            },

            error: () => {
              this.showError(
                'Failed to permanently delete selected branches.'
              );
            }
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

          this.showSuccess(
            'Branch import completed successfully.'
          );

          this.load(false);
        }
      });
  }

  /* =========================================================
     DELETE
  ========================================================= */

  delete(
    branch: BranchListItemDTO
  ): void {

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        width: '90vw',
        maxWidth: '1000px',
        panelClass: 'enterprise-dialog',
        autoFocus: false,
        restoreFocus: false,
        data: {
          title: `Delete ${branch.name}`,
          severity: 'warning',

          message:
            'Select the deletion strategy that best matches what you want to remove.',

          confirmText: 'Delete Branch',

          options: [
            {
              value:
                BranchDeletionMode.SOFT_CONFIGURATION_ONLY,

              label:
                'Configuration Only',

              collapsible: true,
              initiallyExpanded: true,

              description:
                'Remove branch configuration while preserving operational history.',

              deletes: [
                'Products',
                'Product Variants',
                'Product Images',
                'Categories',
                'Suppliers',
                'Customers',
                'Inventory Structure',
                'Branch Settings',
                'Notification Settings'
              ],

              keeps: [
                'Sales',
                'Purchases',
                'Payments',
                'Accounting',
                'Tax Records',
                'Attendance',
                'Audit Logs'
              ]
            },

            {
              value:
                BranchDeletionMode.SOFT_FULL,

              label:
                'Full Soft Delete',

              collapsible: true,
              initiallyExpanded: true,

              description:
                'Remove branch operational and configuration data while allowing future restoration.',

              deletes: [
                'Configuration Data',
                'Sales',
                'Purchases',
                'Inventory',
                'Accounting',
                'Tax',
                'Messaging',
                'Attendance'
              ],

              keeps: [
                'Branch Record For Restoration'
              ]
            }
          ],

          selectedOption:
            BranchDeletionMode.SOFT_FULL,

          requireAcknowledgement: true,

          acknowledgementText:
            'I understand the selected deletion action.'
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.branchService
          .delete(
            branch.id,
            result.option
          )
          .subscribe({
            next: () => {

              this.clearSelection();

              this.showSuccess(
                `"${branch.name}" deleted successfully.`
              );

              this.load(false);
            },

            error: () => {
              this.showError(
                `Failed to delete "${branch.name}".`
              );
            }
          });

      });
  }

  hardDelete(
    branch: BranchListItemDTO
  ): void {

    const ref = this.dialog.open(
      ConfirmDialogComponent,
      {
        width: '900px',
        maxWidth: '95vw',
        panelClass: 'enterprise-dialog',
        autoFocus: false,
        restoreFocus: false,

        data: {
          title:
            `Permanently Delete ${branch.name}`,

          severity: 'danger',

          message:
            'This branch has already been soft deleted. Permanent deletion cannot be undone.',

          details: [
            'All remaining branch data will be permanently removed.',
            'Associated storage files will be deleted.',
            'The branch cannot be restored afterwards.'
          ],

          confirmText:
            'Delete Permanently',

          requireAcknowledgement: true,

          acknowledgementText:
            'I understand this operation is irreversible.'
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.branchService
          .delete(
            branch.id,
            BranchDeletionMode.HARD
          )
          .subscribe({
            next: () => {

              this.showSuccess(
                `"${branch.name}" permanently deleted.`
              );

              this.load(false);
            },

            error: () => {
              this.showError(
                `Failed to permanently delete "${branch.name}".`
              );
            }
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
      .subscribe({
        next: () => {

          this.selectedIds.delete(
            branch.id
          );

          this.showSuccess(
            `"${branch.name}" restored successfully.`
          );

          this.load(false);
        },

        error: () => {
          this.showError(
            `Failed to restore "${branch.name}".`
          );
        }
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

  private showSuccess(
    message: string
  ): void {

    this.snackBar.open(
      message,
      'Dismiss',
      {
        duration: 4000,
        panelClass: ['snackbar-success']
      }
    );
  }

  private showError(
    message: string
  ): void {

    this.snackBar.open(
      message,
      'Dismiss',
      {
        duration: 6000,
        panelClass: ['snackbar-error']
      }
    );
  }
}