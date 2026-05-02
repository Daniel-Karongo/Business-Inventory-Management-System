import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatTabsModule } from '@angular/material/tabs';

import { User } from '../../models/user.model';
import { UserService } from '../../services/user/user.service';

import { UserImageAdapter } from '../../services/user/user-image.adapter';

import { UserAuditTimelineComponent } from '../user-audit-timeline/user-audit-timeline.component';
import { UserRollcallsComponent } from '../user-rollcalls/user-rollcalls.component';

import { BiometricManagerComponent } from '../../../../../../shared/components/biometric-manager/biometric-manager.component';
import { EntityImageManagerComponent } from '../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { EntityActionConfig, EntityActionService } from '../../../../../../shared/services/entity-action.service';
import { AuthService } from '../../../../../auth/services/auth.service';
import {
  USER_ACTION_REASONS,
  UserActionType
} from '../../models/user-action-reasons.model';
import { firstValueFrom, take } from 'rxjs';
import * as pdfjsLib from 'pdfjs-dist';
import { UserThumbnailService } from '../../services/user/user-thumbnail.service';

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
    UserRollcallsComponent,
    BiometricManagerComponent
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

  thumbnailObjectUrl = '';
  currentUserUsername = '';
  currentUserRole = '';

  private actionConfig!: EntityActionConfig<User>;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private userService: UserService,
    private auth: AuthService,
    private snackbar: MatSnackBar,
    private entityAction: EntityActionService,
    private thumbnailService: UserThumbnailService
  ) { }

  ngOnInit() {

    const username = this.route.snapshot.paramMap.get('username');

    if (!username) return;

    this.auth.getCurrentUser().subscribe(u => {
      this.role = u?.role ?? '';
      this.currentUserRole = u?.role ?? '';
      this.currentUserUsername = u?.username ?? '';
    });

    this.imageAdapter = UserImageAdapter(
      this.userService,
      async () => {
        this.thumbnailService.invalidate(this.user.id!);

        // 🔥 MUST refresh user to get new thumbnail URL
        const updated = await firstValueFrom(
          this.userService.get(this.user.username, null)
        );

        this.user = updated;

        await this.loadThumbnail(updated);
      },
      async () => {
        this.thumbnailService.invalidate(this.user.id!);

        const updated = await firstValueFrom(
          this.userService.get(this.user.username, null)
        );

        this.user = updated;

        this.loadCounts();
        this.loadThumbnail(updated);
      }
    );

    this.userService.get(username, null).subscribe({
      next: u => {
        this.user = u;

        this.loadThumbnail(u);
        this.loading = false;
        this.initializeActionConfig();
        this.loadCounts();
      }
    });
  }

  hasThumbnail(): boolean {
    return !!this.user?.profileThumbnailUrl;
  }

  thumbnailUrl(): string {
    if (!this.user?.profileThumbnailUrl) {
      return '';
    }

    return this.user.profileThumbnailUrl.startsWith('/api')
      ? this.user.profileThumbnailUrl
      : `/api${this.user.profileThumbnailUrl}`;
  }

  departmentCount(): number {

    return (this.user?.branchHierarchy || [])
      .reduce(
        (sum: number, b: any) =>
          sum + (b?.departments?.length || 0),
        0
      );

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

      reload: () => this.reloadUserDetails()
    };
  }

  private reloadUserDetails() {

    if (!this.user?.username) return;

    this.loading = true;

    this.userService.get(this.user.username, null).subscribe({
      next: u => {
        this.user = u;

        // ✅ refresh thumbnail
        this.loadThumbnail(u);

        // ✅ refresh counts
        this.loadCounts();

        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  private loadThumbnail(u: User) {
    this.thumbnailObjectUrl = '';

    if (!u.profileThumbnailUrl) return;

    this.thumbnailService
      .getThumbnail(u.id!, u.profileThumbnailUrl)
      .pipe(take(1))
      .subscribe(url => {
        this.thumbnailObjectUrl = url;
      });
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

  isSelf(): boolean {
    return this.user?.username === this.currentUserUsername;
  }

  canViewBiometrics(): boolean {
    return this.isSelf();
  }

  roleRank(role: string): number {
    return ['EMPLOYEE', 'SUPERVISOR', 'MANAGER', 'ADMIN', 'SUPERUSER'].indexOf(role);
  }

  canToggleUser(): boolean {
    if (!this.canManageUser()) return false;
    if (this.isSelf()) return false;

    const targetRank = this.roleRank(this.user.role);
    const currentRank = this.roleRank(this.currentUserRole);

    return targetRank < currentRank;
  }

  /* ================= BADGE COUNTS ================= */

  private loadCounts() {

    if (this.canViewAudits()) {
      this.userService.auditsForUser(this.user.id!).subscribe(a =>
        this.auditCount = a?.length || 0
      );
    }

    if (this.canViewImages()) {
      this.userService.listImages(this.user.id!).subscribe({
        next: i => this.imageCount = i?.length || 0,
        error: () => this.imageCount = 0 // 👈 suppress error
      });
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
    this.router.navigate(
      ['edit'],
      { relativeTo: this.route }
    );
  }

  /* ================= ACTION HANDLER ================= */

  toggleUser() {

    if (!this.user) return;

    this.entityAction.toggleSingle(
      this.user,
      this.actionConfig
    );
  }

  ngOnDestroy() {
    if (this.thumbnailObjectUrl) {
      URL.revokeObjectURL(this.thumbnailObjectUrl);
    }

    if (this.user?.id) {
      this.thumbnailService.invalidate(this.user.id); // ✅ cleanup cache
    }
  }
}