import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatDialog } from '@angular/material/dialog';

import { AclRoleMatrixService } from '../acl-role-matrix.service';
import { AclAdminService } from '../../../services/acl-admin.service';
import { AclChangeReviewComponent } from '../../../components/acl-change-review/acl-change-review.component';
import { forkJoin } from 'rxjs';

@Component({
  selector: 'app-acl-role-matrix',
  standalone: true,
  imports: [CommonModule, MatTooltipModule],
  templateUrl: './acl-role-matrix.component.html',
  styleUrls: ['./acl-role-matrix.component.scss']
})
export class AclRoleMatrixComponent implements OnInit {

  private svc = inject(AclRoleMatrixService);

  roles: any[] = [];
  permissions: any[] = [];
  grants = new Set<string>();

  editMode = false;
  loadingMatrix = true;

  pendingChanges: {
    role: string;
    permission: string;
    newValue: boolean;
  }[] = [];

  private readonly ROLE_ORDER = [
    'SUPERUSER',
    'ADMIN',
    'MANAGER',
    'SUPERVISOR',
    'EMPLOYEE'
  ];

  constructor(
    private dialog: MatDialog,
    private aclAdmin: AclAdminService
  ) { }

  ngOnInit(): void {
    this.load();
  }

  load() {
    this.loadingMatrix = true;

    forkJoin({
      roles: this.svc.roles(),
      perms: this.svc.permissions(),
      grants: this.svc.rolePermissions()
    }).subscribe(({ roles, perms, grants }) => {
      this.roles = roles;
      this.permissions = perms;
      this.grants.clear();

      grants
        .filter(rp => rp.active && rp.allowed)
        .forEach(rp =>
          this.grants.add(`${rp.role.name}:${rp.permission.code}`)
        );

      this.loadingMatrix = false;
    });
  }

  orderedRoles() {
    return [...this.roles].sort(
      (a, b) =>
        this.ROLE_ORDER.indexOf(a.name) -
        this.ROLE_ORDER.indexOf(b.name)
    );
  }

  groupedPermissions() {
    const groups: Record<string, any[]> = {};

    for (const p of this.permissions) {
      const module = this.extractModule(p.code);
      groups[module] ??= [];
      groups[module].push(p);
    }

    return Object.entries(groups).sort(([a], [b]) => a.localeCompare(b));
  }

  has(role: string, perm: string) {
    return this.grants.has(`${role}:${perm}`);
  }

  toggleEdit() {
    this.editMode = true;
  }

  cancelEdit() {
    this.editMode = false;
    this.pendingChanges = [];
  }

  toggle(role: string, perm: string) {
    if (!this.editMode) return;

    const exists = this.has(role, perm);

    this.pendingChanges = this.pendingChanges.filter(
      c => !(c.role === role && c.permission === perm)
    );

    this.pendingChanges.push({
      role,
      permission: perm,
      newValue: !exists
    });
  }

  isPending(role: string, perm: string) {
    return this.pendingChanges.some(
      c => c.role === role && c.permission === perm
    );
  }

  isPendingAdd(role: string, perm: string) {
    return this.pendingChanges.some(
      c => c.role === role && c.permission === perm && c.newValue
    );
  }

  isPendingRemove(role: string, perm: string) {
    return this.pendingChanges.some(
      c => c.role === role && c.permission === perm && !c.newValue
    );
  }

  pendingTooltip(role: string, perm: string) {
    if (this.isPendingAdd(role, perm)) return 'Will be granted';
    if (this.isPendingRemove(role, perm)) return 'Will be revoked';
    return '';
  }

  reviewChanges() {
    const ref = this.dialog.open(AclChangeReviewComponent, {
      minWidth: '520px',
      maxWidth: '80vw',
      data: { changes: this.pendingChanges }
    });

    ref.afterClosed().subscribe(reason => {
      if (!reason) return;

      this.aclAdmin.apply(this.pendingChanges, reason)
        .subscribe(() => {
          this.editMode = false;
          this.pendingChanges = [];
          this.load();
        });
    });
  }

  private extractModule(code: string): string {
    const match = code.match(/API_[A-Z]+_API_([^_]+)/);
    return match ? match[1] : 'OTHER';
  }

  humanize(code: string): string {
    return code
      .replace(/^API_[A-Z]+_API_/, '')
      .replace(/_/g, ' ')
      .toLowerCase()
      .replace(/\b\w/g, c => c.toUpperCase());
  }
}