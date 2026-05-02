interface UserVM extends User {
  thumbnail?: string;
  thumbnailLoading?: boolean;
}

import { CommonModule } from '@angular/common';
import { Component, OnInit, ViewChild } from '@angular/core';
import { Router, RouterModule } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatSelectModule } from '@angular/material/select';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';

import { BreakpointObserver } from '@angular/cdk/layout';

import {
  BehaviorSubject,
  combineLatest,
  debounceTime,
  distinctUntilChanged,
  switchMap,
  take,
  tap
} from 'rxjs';

import { User } from '../../models/user.model';
import { UserService } from '../../services/user/user.service';

import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';
import { RoleService } from '../../services/role/role.service';

import { EntityActionConfig, EntityActionService } from '../../../../../../shared/services/entity-action.service';
import { USER_ACTION_REASONS, UserActionType } from '../../models/user-action-reasons.model';

import { AuthService } from '../../../../../auth/services/auth.service';

import { LazyLoadDirective } from '../../../../../../shared/directives/lazy-load.directive';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { UserThumbnailService } from '../../services/user/user-thumbnail.service';
import { UserBulkImportDialogComponent } from '../../components/user-bulk-import-dialog/user-bulk-import-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { FileViewerDialog } from '../../../../../../shared/components/file-viewer/file-viewer.component';

@Component({
  selector: 'app-user-list',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    PageShellComponent,
    MatTableModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule,
    MatPaginatorModule,
    MatTooltipModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    LazyLoadDirective
  ],
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {

  /* =========================
     SERVER + UI PAGINATION MODEL (MATCH PRODUCT)
  ========================= */

  private page$ = new BehaviorSubject<number>(0);
  private serverSize$ = new BehaviorSubject<number>(100);

  private uiPage$ = new BehaviorSubject<number>(0);
  uiPageSize$ = new BehaviorSubject<number>(20);

  private loadedServerPages = new Set<number>();

  /* =========================
     FILTER STREAMS
  ========================= */

  private search$ = new BehaviorSubject<string>('');
  private roleFilter$ = new BehaviorSubject<string | null>(null);
  private branchFilter$ = new BehaviorSubject<string | null>(null);
  private departmentFilter$ = new BehaviorSubject<string | null>(null);

  private statusFilter$ = new BehaviorSubject<'all' | 'active' | 'deleted'>('all');
  private showDeleted$ = new BehaviorSubject<boolean>(false);

  private sortField$ = new BehaviorSubject<string | null>(null);
  private sortDir$ = new BehaviorSubject<'asc' | 'desc'>('asc');

  refresh$ = new BehaviorSubject<void>(undefined);

  /* =========================
     DATA STATE
  ========================= */

  allUsers: UserVM[] = [];
  users: UserVM[] = [];

  totalServerElements = 0;
  loading = true;

  selectedIds = new Set<string>();
  skeletons = Array.from({ length: 12 });

  /* =========================
     UI STATE
  ========================= */

  displayedColumns = [
    'select',
    'username',
    'emails',
    'phones',
    'branches_depts',
    'role',
    'status',
    'created',
    'actions'
  ];

  branches: any[] = [];
  departments: any[] = [];
  roles: any[] = [];

  viewMode: 'table' | 'grid' = 'table';
  density: 'compact' | 'comfortable' = 'compact';

  isMobile = false;
  allowShowDeleted = false;

  private readonly STORAGE_KEY = 'user_list_prefs';

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  /* =========================
     ACTION CONFIG
  ========================= */

  private userActionConfig: EntityActionConfig<User> = {
    entityName: 'User',
    displayName: (u) => u.username,

    disableReasons: USER_ACTION_REASONS[UserActionType.DISABLE],
    restoreReasons: USER_ACTION_REASONS[UserActionType.RESTORE],
    deleteReasons: USER_ACTION_REASONS[UserActionType.DELETE],

    disable: (id, reason) => this.userService.softDelete(id, reason),
    restore: (id, reason) => this.userService.restore(id, reason),
    hardDelete: (id, reason) => this.userService.hardDelete(id, reason),

    disableBulk: (ids, reason) => this.userService.softDeleteBulk(ids, reason),
    restoreBulk: (ids, reason) => this.userService.restoreBulk(ids, reason),
    hardDeleteBulk: (ids, reason) => this.userService.hardDeleteBulk(ids, reason),

    reload: () => {
      this.resetAccumulation();
      this.page$.next(0);
      this.refresh$.next();
      this.clearSelection();
    }
  };

  constructor(
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private roleService: RoleService,
    private entityAction: EntityActionService,
    private auth: AuthService,
    private breakpoint: BreakpointObserver,
    private router: Router,
    private thumbnailService: UserThumbnailService,
    private dialog: MatDialog
  ) { }

  /* =========================
     INIT
  ========================= */

  ngOnInit(): void {

    const me = this.auth.getSnapshot();
    this.allowShowDeleted =
      !!me && ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(me.role);

    this.loadFilters();
    this.loadPreferences();

    this.breakpoint.observe(['(max-width: 640px)']).subscribe(res => {
      this.isMobile = res.matches;

      if (this.isMobile) {
        this.viewMode = 'grid'; // ✅ correct
      }
    });

    const data$ = combineLatest([
      this.page$,
      this.serverSize$,
      this.search$.pipe(debounceTime(800), distinctUntilChanged()),
      this.roleFilter$,
      this.branchFilter$,
      this.departmentFilter$,
      this.statusFilter$,
      this.showDeleted$,
      this.sortField$,
      this.sortDir$,
      this.refresh$
    ]).pipe(
      tap(() => this.loading = true),
      switchMap(([page, size, search, role, branch, dept, status, showDeleted, sortField, sortDir]) => {

        const filter: any = {};

        if (search) filter.q = search;
        if (role) filter.role = role;
        if (branch) filter.branch = branch;
        if (dept) filter.department = dept;

        // ✅ Correct logic
        if (showDeleted) {
          // show both
          filter.deleted = null;
        } else {
          if (status === 'active') filter.deleted = false;
          else if (status === 'deleted') filter.deleted = true;
          else filter.deleted = false; // ✅ DEFAULT = active only
        }

        return this.userService.list(page, size, filter, sortField, sortDir);
      }),
      tap(res => {

        const currentPage = this.page$.value;

        if (!this.loadedServerPages.has(currentPage)) {

          const batch: UserVM[] = res.data.map(u => ({
            ...u,
            thumbnail: '',
            thumbnailLoading: false
          }));

          this.allUsers = [...this.allUsers, ...batch];
          this.loadedServerPages.add(currentPage);
        }

        this.totalServerElements = res.total;
        this.applyClientPagination();
        this.loading = false;
      })
    );

    data$.subscribe();
  }

  getDepartmentCount(u: UserVM): number {
    return (u.branchHierarchy || [])
      .reduce((sum, b) => sum + (b.departments?.length || 0), 0);
  }

  /* =========================
     PAGINATION
  ========================= */

  private applyClientPagination() {
    const start = this.uiPage$.value * this.uiPageSize$.value;
    const end = start + this.uiPageSize$.value;
    this.users = this.allUsers.slice(start, end);
  }

  changePage(e: PageEvent) {

    const requestedIndex = e.pageIndex;
    const size = e.pageSize;

    this.uiPageSize$.next(size);

    const requiredIndex = requestedIndex * size;

    if (requiredIndex < this.allUsers.length) {
      this.uiPage$.next(requestedIndex);
      this.applyClientPagination();
      return;
    }

    const serverPage =
      Math.floor(requiredIndex / this.serverSize$.value);

    if (!this.loadedServerPages.has(serverPage)) {
      this.page$.next(serverPage);
    }

    this.uiPage$.next(requestedIndex);
  }

  private resetAccumulation() {
    this.allUsers = [];
    this.loadedServerPages.clear();
    this.uiPage$.next(0);
  }

  /* =========================
     FILTERS
  ========================= */

  onSearch(v: string) {
    this.resetAccumulation();
    this.page$.next(0);
    this.search$.next(v);
  }

  onRole(v: string | null) {
    this.resetAccumulation();
    this.page$.next(0);
    this.roleFilter$.next(v);
  }

  onBranch(v: string | null) {
    this.resetAccumulation();
    this.page$.next(0);
    this.branchFilter$.next(v);
  }

  onDepartment(v: string | null) {
    this.resetAccumulation();
    this.page$.next(0);
    this.departmentFilter$.next(v);
  }

  onStatus(v: 'all' | 'active' | 'deleted') {
    this.resetAccumulation();
    this.page$.next(0);
    this.statusFilter$.next(v);
  }

  onShowDeleted(v: boolean) {
    this.resetAccumulation();
    this.page$.next(0);
    this.showDeleted$.next(v);
  }

  /* =========================
     SORT
  ========================= */

  sortBy(field: string) {

    if (this.sortField$.value === field) {
      this.sortDir$.next(
        this.sortDir$.value === 'asc' ? 'desc' : 'asc'
      );
    } else {
      this.sortField$.next(field);
      this.sortDir$.next('asc');
    }

    this.resetAccumulation();
    this.page$.next(0);
  }

  getSortIcon(field: string): string {
    if (this.sortField$.value !== field) return 'unfold_more';
    return this.sortDir$.value === 'asc'
      ? 'arrow_upward'
      : 'arrow_downward';
  }

  /* =========================
     SELECTION
  ========================= */

  toggleRowSelection(u: UserVM) {
    this.selectedIds.has(u.id)
      ? this.selectedIds.delete(u.id)
      : this.selectedIds.add(u.id);
  }

  isAllSelected() {
    return this.users.every(u => this.selectedIds.has(u.id));
  }

  masterToggle() {
    if (this.isAllSelected()) {
      this.users.forEach(u => this.selectedIds.delete(u.id));
    } else {
      this.users.forEach(u => this.selectedIds.add(u.id));
    }
  }

  clearSelection() {
    this.selectedIds.clear();
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    if (!this.selectedIds.size) return null;

    const states = new Set(
      this.users.filter(u => this.selectedIds.has(u.id)).map(u => u.deleted)
    );

    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }

  /* =========================
     ACTIONS
  ========================= */

  openImage(u: UserVM) {
    if (!u.thumbnail) return;

    this.dialog.open(FileViewerDialog, {
      data: {
        preview: {
          src: u.thumbnail,
          name: u.username,
          type: 'image'
        }
      },
      width: '80%',
      maxWidth: '1100px'
    });
  }

  toggleUser(u: UserVM) {
    this.entityAction.toggleSingle(u, this.userActionConfig);
  }

  bulkDisable() {
    this.entityAction.bulkDisable(this.getSelected(), this.userActionConfig);
  }

  bulkRestore() {
    this.entityAction.bulkRestore(this.getSelected(), this.userActionConfig);
  }

  bulkDelete() {
    this.entityAction.bulkHardDelete(this.getSelected(), this.userActionConfig);
  }

  private getSelected() {
    return this.allUsers.filter(u => this.selectedIds.has(u.id));
  }

  openBulkImport() {
    this.dialog.open(UserBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    })
      .afterClosed()
      .subscribe(result => {
        if (result === true) {
          // ✅ Proper refresh for your reactive pipeline
          this.resetAccumulation();
          this.page$.next(0);
          this.refresh$.next();
        }
      });
  }

  /* =========================
     NAVIGATION
  ========================= */

  view(u: UserVM) {
    this.router.navigate(['/app/users', u.username]);
  }

  edit(u: UserVM) {
    this.router.navigate(['/app/users', u.username, 'edit']);
  }

  /* =========================
     THUMBNAIL (PDF SUPPORT)
  ========================= */

  loadThumbnail(u: UserVM) {
    if (u.thumbnailLoading || u.thumbnail) return;
    if (!u.profileThumbnailUrl || !u.id) return;

    u.thumbnailLoading = true;

    this.thumbnailService
      .getThumbnail(u.id, u.profileThumbnailUrl)
      .pipe(take(1))
      .subscribe({
        next: (url) => {
          u.thumbnail = url;
          u.thumbnailLoading = false;
        },
        error: () => {
          u.thumbnailLoading = false;
        }
      });
  }


  /* =========================
     FILTER LOAD
  ========================= */

  loadFilters() {
    this.branchService.getAll().subscribe(b => this.branches = b || []);
    this.departmentService.getAll().subscribe(d => this.departments = d || []);
    this.roleService.list().subscribe(r => this.roles = r || []);
  }

  /* =========================
     PREFS
  ========================= */

  savePreferences() {
    localStorage.setItem(this.STORAGE_KEY, JSON.stringify({
      viewMode: this.viewMode,
      density: this.density
    }));
  }

  private loadPreferences() {
    const raw = localStorage.getItem(this.STORAGE_KEY);
    if (!raw) return;

    const p = JSON.parse(raw);
    this.viewMode = p.viewMode ?? this.viewMode;
    this.density = p.density ?? this.density;
  }
}