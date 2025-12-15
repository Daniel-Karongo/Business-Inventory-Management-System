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

import { UserImageManagerComponent } from '../user-image-manager/user-image-manager.component';
import { UserAuditTimelineComponent } from '../user-audit-timeline/user-audit-timeline.component';
import { UserRollcallsComponent } from '../user-rollcalls/user-rollcalls.component';

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
    UserImageManagerComponent,
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

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private userService: UserService,
    private auth: AuthService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit() {
    const username = this.route.snapshot.paramMap.get('username');
    if (!username) return;

    this.role = this.auth.getRole();

    this.userService.get(username).subscribe({
      next: u => {
        this.user = u;
        this.loading = false;
        this.loadCounts();
      },
      error: () => {
        this.snackbar.open('Failed to load user', 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  /* ---------------- Role Guards ---------------- */

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

  /* ---------------- Badge Counts ---------------- */

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

  /* ---------------- UI ---------------- */

  initials(): string {
    return this.user.username.charAt(0).toUpperCase();
  }

  status(): string {
    return this.user.deleted ? "DISABLED" : "ACTIVE";
  }

  editUser() {
    this.router.navigate(['/users', this.user.username, 'edit']);
  }

  /* User Functions */

  canManageUser(): boolean {
    return ['ADMIN', 'SUPERUSER'].includes(this.role);
  }

  deleteUser() {
    if (!confirm('Disable this user?')) return;

    this.userService.softDelete(this.user.id!, 'Disabled from user details')
      .subscribe({
        next: () => {
          this.user.deleted = true;
          this.snackbar.open('User disabled', 'Close', { duration: 2000 });
        }
      });
  }

  restoreUser() {
    if (!confirm('Restore this user?')) return;

    this.userService.restore(this.user.id!, 'Restored from user details')
      .subscribe({
        next: () => {
          this.user.deleted = false;
          this.snackbar.open('User restored', 'Close', { duration: 2000 });
        }
      });
  }
}