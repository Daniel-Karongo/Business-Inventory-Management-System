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
import { firstValueFrom } from 'rxjs';
import * as pdfjsLib from 'pdfjs-dist';

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
    private entityAction: EntityActionService
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
      () => {
        this.userService.get(this.user.username, null).subscribe(u => {
          this.user = u;
          this.loadThumbnail(u);
        });
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

        this.imageAdapter = UserImageAdapter(
          this.userService,
          () => {
            this.userService.get(this.user.username, null).subscribe(u => {
              this.user = u;
              this.loadThumbnail(u);
            });
          }
        );

        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  private async loadThumbnail(u: User) {

    if (!u.profileThumbnailUrl) {
      this.thumbnailObjectUrl = '';
      return;
    }

    const fileName = u.profileThumbnailUrl.split('/').pop()!;
    const ext = fileName.split('.').pop()?.toLowerCase();

    const blob = await firstValueFrom(
      this.userService.getUserImageBlob(u.id!, fileName, null)
    );

    if (this.thumbnailObjectUrl) {
      URL.revokeObjectURL(this.thumbnailObjectUrl);
    }

    // ✅ IMAGE → direct render
    if (['jpg', 'jpeg', 'png', 'gif', 'webp'].includes(ext!)) {
      this.thumbnailObjectUrl = URL.createObjectURL(blob);
      return;
    }

    // ✅ PDF → generate thumbnail (same as manager)
    if (ext === 'pdf') {
      this.thumbnailObjectUrl =
        await this.generatePdfThumbnail(blob);
    }
  }

  private async generatePdfThumbnail(blob: Blob): Promise<string> {
    try {
      const pdf = await pdfjsLib.getDocument({
        data: await blob.arrayBuffer()
      }).promise;

      const page = await pdf.getPage(1);

      const viewport = page.getViewport({ scale: 0.5 });

      const canvas = document.createElement('canvas');
      const context = canvas.getContext('2d');

      if (!context) return '';

      canvas.width = viewport.width;
      canvas.height = viewport.height;

      await page.render({
        canvasContext: context,
        viewport
      } as any).promise;

      return canvas.toDataURL();

    } catch (e) {
      console.error('PDF thumbnail failed:', e);
      return '';
    }
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
  }
}