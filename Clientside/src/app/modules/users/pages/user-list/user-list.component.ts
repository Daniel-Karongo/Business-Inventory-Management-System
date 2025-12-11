import { Component, OnInit, ViewChild } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { MatTableModule } from '@angular/material/table';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSelectModule } from '@angular/material/select';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatSortModule } from '@angular/material/sort';
import { SelectionModel } from '@angular/cdk/collections';

import { Router } from '@angular/router';
import { finalize } from 'rxjs/operators';

import { UserService } from '../../services/user/user.service';
import { User } from '../../models/user.model';

import { ReasonDialogComponent } from '../../../../shared/components/reason-dialog/reason-dialog.component';
import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';
import { RoleService } from '../../services/role/role.service';

@Component({
  selector: 'app-user-list',
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatTableModule,
    MatPaginatorModule,
    MatIconModule,
    MatButtonModule,
    MatSnackBarModule,
    MatTooltipModule,
    MatSelectModule,
    MatDialogModule,
    MatCheckboxModule,
    MatSortModule
  ],
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {

  displayedColumns = ['select', 'username', 'emails', 'phones', 'branches_depts', 'role', 'status', 'createdAt', 'actions'];

  /** Holds all users loaded once from backend */
  allUsers: User[] = [];

  /** After applying filters + search + sort */
  filteredUsers: User[] = [];

  /** Users visible on the current page */
  users: User[] = [];

  branches: any[] = [];
  departments: any[] = [];
  roles: any[] = [];
  
  total = 0;
  page = 0;
  size = 12;

  q = '';
  filterBranch = '';
  filterDepartment = '';
  filterRole = '';
  filterStatus: '' | 'active' | 'deleted' = '';
  showDeleted = false;
  allowShowDeleted = false;

  loading = false;

  // selection for bulk actions
  selection = new SelectionModel<User>(true, []);

  // sorting
  sortField: string | null = null;
  sortDir: 'asc' | 'desc' = 'asc';

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  constructor(
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private roleService: RoleService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    public router: Router
  ) {}

  ngOnInit() {
    this.determineDeletedVisibility();
    this.loadFilters();
    this.loadUsersOnce();
  }

  determineDeletedVisibility() {
    const session = localStorage.getItem('auth_roles');

    if (!session) {
      this.allowShowDeleted = false;
      return;
    }

    try {
      const roles = JSON.parse(session);
      const primary = Array.isArray(roles) ? roles[0] : roles;
      this.allowShowDeleted = ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(primary);
      // default showDeleted false; user can toggle it via UI if allowed
    } catch {
      this.allowShowDeleted = false;
    }
  }

  loadFilters() {
    this.branchService.getAll().subscribe(b => (this.branches = b || []));
    this.departmentService.getAll().subscribe(d => (this.departments = d || []));
    this.roleService.list().subscribe(r => (this.roles = r || []));
  }

  loadUsersOnce() {
    this.loading = true;
    // request a large page so backend returns everything (we paginate client-side)
    this.userService.list(0, 999999, {}).pipe(
      finalize(() => this.loading = false)
    ).subscribe({
      next: r => {
        this.allUsers = r.data || [];
        // initialize defaults
        this.filteredUsers = [...this.allUsers];
        this.total = this.filteredUsers.length;
        this.applyFilters(); // will set `users`
      },
      error: () => {
        this.snackbar.open('Failed to load users', 'Close', { duration: 3000 });
      }
    });
  }

  applyFilters() {
    let data = [...this.allUsers];

    // search - matches username, any email, any phone
    const q = (this.q || '').trim().toLowerCase();
    if (q) {
      data = data.filter(u =>
        (u.username || '').toLowerCase().includes(q) ||
        (u.emailAddresses || []).some(e => e.toLowerCase().includes(q)) ||
        (u.phoneNumbers || []).some(p => p.includes(q))
      );
    }

    // branch filter (checks branchHierarchy)
    if (this.filterBranch) {
      data = data.filter(u =>
        (u.branchHierarchy || []).some(b => b.branchId === this.filterBranch)
      );
    }

    // department filter (checks each branch -> departments entries)
    if (this.filterDepartment) {
      data = data.filter(u =>
        (u.branchHierarchy || []).some(b =>
          (b.departments || []).some(d => d.departmentId === this.filterDepartment)
        )
      );
    }

    // role filter
    if (this.filterRole) {
      data = data.filter(u => u.role === this.filterRole);
    }

    // STATUS FILTER
    if (this.filterStatus === 'active') {
      data = data.filter(u => u.deleted === false);
    }
    else if (this.filterStatus === 'deleted') {
      data = data.filter(u => u.deleted === true);
    }

    // Show Deleted toggle (only hide deleted when no explicit status filter)
    if (!this.showDeleted && this.filterStatus === '') {
      data = data.filter(u => !u.deleted);
    }

    // apply sorting (client-side)
    if (this.sortField) {
      data.sort((a, b) => this.compareForField(a, b, this.sortField!, this.sortDir));
    }

    this.filteredUsers = data;
    this.total = data.length;

    // reset selection (don't keep stale selected items)
    this.selection.clear();

    // apply pagination
    this.applyPagination();
  }

  compareForField(a: User, b: User, field: string, dir: 'asc' | 'desc') {
    const multiplier = dir === 'asc' ? 1 : -1;
    let v1: any = null;
    let v2: any = null;

    switch (field) {
      case 'username':
        v1 = (a.username || '').toLowerCase();
        v2 = (b.username || '').toLowerCase();
        break;

      case 'role':
        v1 = (a.role || '').toLowerCase();
        v2 = (b.role || '').toLowerCase();
        break;

      case 'createdAt':
        v1 = a.createdAt ? new Date(a.createdAt).getTime() : 0;
        v2 = b.createdAt ? new Date(b.createdAt).getTime() : 0;
        break;

      case 'email':
        v1 = (a.emailAddresses && a.emailAddresses[0]) ? a.emailAddresses[0].toLowerCase() : '';
        v2 = (b.emailAddresses && b.emailAddresses[0]) ? b.emailAddresses[0].toLowerCase() : '';
        break;

      case 'phone':
        v1 = (a.phoneNumbers && a.phoneNumbers[0]) ? a.phoneNumbers[0] : '';
        v2 = (b.phoneNumbers && b.phoneNumbers[0]) ? b.phoneNumbers[0] : '';
        break;

      case 'department':
        // pick first department name (or empty)
        v1 = this.firstDepartmentName(a).toLowerCase();
        v2 = this.firstDepartmentName(b).toLowerCase();
        break;

      case 'branch':
        // pick first branch name
        v1 = (a.branchHierarchy && a.branchHierarchy[0]) ? (a.branchHierarchy[0].branchName || '') : '';
        v2 = (b.branchHierarchy && b.branchHierarchy[0]) ? (b.branchHierarchy[0].branchName || '') : '';
        v1 = v1.toLowerCase();
        v2 = v2.toLowerCase();
        break;

      case 'status':
        v1 = (a.deleted || '');
        v2 = (b.deleted || '');
        break;

      default:
        v1 = '';
        v2 = '';
    }

    if (v1 < v2) return -1 * multiplier;
    if (v1 > v2) return 1 * multiplier;
    return 0;
  }

  firstDepartmentName(u: User): string {
    if (!u.branchHierarchy || !u.branchHierarchy.length) return '';
    for (const b of u.branchHierarchy) {
      if (b.departments && b.departments.length) {
        const d = b.departments[0];
        // departmentName may exist according to updated DTO
        return (d.departmentName || d.departmentId || '') as string;
      }
    }
    return '';
  }

  sortBy(field: string) {
    if (this.sortField === field) {
      // toggle direction
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }
    this.applyFilters();
  }

  applyPagination() {
    const start = this.page * this.size;
    const end = start + this.size;
    this.users = this.filteredUsers.slice(start, end);
  }

  changePage(event: PageEvent) {
    this.page = event.pageIndex;
    this.size = event.pageSize;
    this.applyPagination();
  }

  // Selection helpers for bulk
  isAllSelected() {
    const numSelected = this.selection.selected.length;
    const numRows = this.users.length;
    return numSelected === numRows && numRows > 0;
  }

  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
    } else {
      this.users.forEach(row => this.selection.select(row));
    }
  }

  checkboxLabel(row?: User): string {
    if (!row) {
      return `${this.isAllSelected() ? 'Deselect' : 'Select'} all`;
    }
    return `${this.selection.isSelected(row) ? 'Deselect' : 'Select'} row ${row.username}`;
  }

  toggleRowSelection(row: any) {
    this.selection.toggle(row);
  }

  // Single-user disable/restore (with reason)
  toggleActive(u: User) {
    let dialogRef: any;
    if(u.deleted) {
      dialogRef = this.dialog.open(ReasonDialogComponent, {
        width: '420px',
        data: {
          title: 'Restore User?',
          message: `Provide a reason for restoring ${u.username}.`,
          action: 'RESTORE',
          confirmText: 'Restore'
        }
      });
    } else {
      dialogRef = this.dialog.open(ReasonDialogComponent, {
        width: '420px',
        data: {
          title: 'Disable User?',
          message: `Provide a reason for disabling ${u.username}.`,
          action: 'DISABLE',
          confirmText: 'Disable'
        }
      });
    }

    dialogRef.afterClosed().subscribe((result: { confirmed: any; reason: any; }) => {
      if (!result?.confirmed) return;
      const reason = result.reason;

      const obs = u.deleted
        ? this.userService.restore(u.id!, reason)
        : this.userService.softDelete(u.id!, reason);

      obs.subscribe({
        next: () => {
          this.snackbar.open(
            u.deleted ? 'User restored' : 'User disabled',
            'Close',
            { duration: 2000 }
          );
          this.loadUsersOnce();
        },
        error: () =>
          this.snackbar.open(
            u.deleted ? 'Restore failed' : 'Disable failed',
            'Close',
            { duration: 3000 }
          )
      });
    });
  }

  // Bulk actions (disable)
  bulkDisable() {
    const selected = this.selection.selected;
    if (!selected.length) {
      this.snackbar.open('Select users first', 'Close', { duration: 1500 });
      return;
    }

    const dialogRef = this.dialog.open(ReasonDialogComponent, {
      width: '480px',
      data: {
        title: 'Disable users?',
        message: `Provide a reason for disabling ${selected.length} user(s).`,
        action: 'DISABLE',
        confirmText: 'Disable'
      }
    });

    dialogRef.afterClosed().subscribe(result => {
      if (!result?.confirmed) return;
      const reason = result.reason;

      const ids = selected.map(s => s.id!).filter(Boolean);
      // backend expects body list for bulk delete
      this.userService.softDeleteBulk(ids, reason).subscribe({
        next: () => {
          this.snackbar.open('Users disabled', 'Close', { duration: 2000 });
          this.loadUsersOnce();
        },
        error: () => this.snackbar.open('Bulk disable failed', 'Close', { duration: 3000 })
      });
    });
  }

  bulkRestore() {
    const selected = this.selection.selected;
    if (!selected.length) {
      this.snackbar.open('Select users first', 'Close', { duration: 1500 });
      return;
    }

    const dialogRef = this.dialog.open(ReasonDialogComponent, {
      width: '480px',
      data: {
        title: 'Restore users?',
        message: `Provide a reason for restoring ${selected.length} user(s).`,
        action: 'RESTORE',
        confirmText: 'Restore'
      }
    });

    dialogRef.afterClosed().subscribe(result => {
      if (!result?.confirmed) return;
      const reason = result.reason;

      const ids = selected.map(s => s.id!).filter(Boolean);
      this.userService.restoreBulk(ids, reason).subscribe({
        next: () => {
          this.snackbar.open('Users restored', 'Close', { duration: 2000 });
          this.loadUsersOnce();
        },
        error: () => this.snackbar.open('Bulk restore failed', 'Close', { duration: 3000 })
      });
    });
  }

  // navigation helpers
  goCreate() { this.router.navigate(['/users/create']); }
  edit(u: User) { this.router.navigate([`/users/${u.id}/edit`]); }
  view(u: User) { this.router.navigate([`/users/${u.id}`]); }

  // Utility used by template to render branch / dept grid
  branchDeptRows(u: User) {
    // returns array of { branchName, departmentName, position }
    const rows: Array<{ branchName: string; departmentName: string; position?: string }> = [];
    (u.branchHierarchy || []).forEach(b => {
      const branchName = b.branchName || '';
      if (!b.departments || !b.departments.length) {
        rows.push({ branchName, departmentName: '—', position: '' });
      } else {
        (b.departments || []).forEach(d => {
          rows.push({
            branchName,
            departmentName: (d as any).departmentName || d.departmentId || '—',
            position: (d as any).position
          });
        });
      }
    });
    return rows;
  }
}