import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTabsModule } from '@angular/material/tabs';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { UserService } from '../../services/user/user.service';
import { AuthService } from '../../../auth/services/auth.service';
import { User } from '../../models/user.model';

import { EntityImageManagerComponent } from
  '../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { UserImageAdapter } from '../../services/user/user-image.adapter';

import { UserAuditTimelineComponent } from '../user-audit-timeline/user-audit-timeline.component';
import { UserRollcallsComponent } from '../user-rollcalls/user-rollcalls.component';

import {
  USER_ACTION_REASONS,
  UserActionType
} from '../../models/user-action-reasons.model';

import {
  EntityActionService,
  EntityActionConfig
} from '../../../../shared/services/entity-action.service';

@Component({
  selector: 'app-user-details',
  standalone: true,
  imports: [
    CommonModule,
    MatCardModule,
    MatButtonModule,
    MatIconModule,
    MatTabsModule,
    MatSnackBarModule,
    EntityImageManagerComponent,
    UserAuditTimelineComponent,
    UserRollcallsComponent
  ],
  templateUrl: './user-details.component.html',
  styleUrls: ['./user-details.component.scss']
})
export class UserDetailsComponent implements OnInit {

  user!: User;
  loading = true;
  role = '';

  auditCount = 0;
  imageCount = 0;
  rollcallCount = 0;
  imageAdapter!: any;

  private actionConfig!: EntityActionConfig<User>;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private userService: UserService,
    private auth: AuthService,
    private snackbar: MatSnackBar,
    private entityAction: EntityActionService
  ) { }

  ngOnInit() {

    const username = this.route.snapshot.paramMap.get('username');
    if (!username) return;

    this.auth.getCurrentUser().subscribe(u => {
      this.role = u?.role ?? '';
    });

    this.imageAdapter = UserImageAdapter(this.userService);

    this.userService.get(username).subscribe({
      next: u => {
        this.user = u;
        this.loading = false;
        this.initializeActionConfig();
        this.loadCounts();
      },
      error: () => {
        this.snackbar.open('Failed to load user', 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  /* ================= ACTION CONFIG ================= */

  private initializeActionConfig() {

    this.actionConfig = {
      entityName: 'User',
      displayName: (u) => u.username,

      disableReasons: USER_ACTION_REASONS[UserActionType.DISABLE],
      restoreReasons: USER_ACTION_REASONS[UserActionType.RESTORE],
      deleteReasons: USER_ACTION_REASONS[UserActionType.DELETE],

      disable: (id, reason) =>
        this.userService.softDelete(id, reason),

      restore: (id, reason) =>
        this.userService.restore(id, reason),

      hardDelete: (id, reason) =>
        this.userService.hardDelete(id, reason),

      reload: () => {
        this.userService.get(this.user.username).subscribe(u => {
          this.user = u;
        });
      }
    };
  }

  /* ================= ROLE GUARDS ================= */

  canViewImages() {
    return ['ADMIN', 'SUPERUSER'].includes(this.role);
  }

  canViewAudits() {
    return ['MANAGER', 'ADMIN', 'SUPERUSER'].includes(this.role);
  }

  canViewRollcalls() {
    return ['SUPERVISOR', 'MANAGER', 'ADMIN', 'SUPERUSER'].includes(this.role);
  }

  canViewUserId(): boolean {
    return this.role === 'SUPERUSER';
  }

  canManageUser(): boolean {
    return ['ADMIN', 'SUPERUSER'].includes(this.role);
  }

  /* ================= BADGE COUNTS ================= */

  private loadCounts() {

    if (this.canViewAudits()) {
      this.userService.auditsForUser(this.user.id!).subscribe(a =>
        this.auditCount = a?.length || 0
      );
    }

    if (this.canViewImages()) {
      this.userService.listImages(this.user.id!).subscribe(i =>
        this.imageCount = i?.length || 0
      );
    }

    if (this.canViewRollcalls()) {
      this.userService.getUserRollcalls(this.user.id!).subscribe(r =>
        this.rollcallCount = r?.length || 0
      );
    }
  }

  /* ================= UI HELPERS ================= */

  initials(): string {
    return this.user.username.charAt(0).toUpperCase();
  }

  status(): string {
    return this.user.deleted ? "DISABLED" : "ACTIVE";
  }

  editUser() {
    this.router.navigate(['/users', this.user.username, 'edit']);
  }

  /* ================= ACTION HANDLER ================= */

  toggleUser() {

    if (!this.user) return;

    this.entityAction.toggleSingle(
      this.user,
      this.actionConfig
    );
  }

}