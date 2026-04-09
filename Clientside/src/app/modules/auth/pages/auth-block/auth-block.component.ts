import { Component, computed, inject, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { AuthService } from '../../services/auth.service';
import { timer, Subject, switchMap, takeUntil } from 'rxjs';
import { finalize } from 'rxjs/operators';
import { resolveTenantLanding } from '../../../../core/utils/role-landing.util';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';

@Component({
  standalone: true,
  selector: 'app-auth-block',
  imports: [CommonModule, MatButtonModule, MatProgressSpinnerModule],
  templateUrl: './auth-block.component.html',
  styleUrls: ['./auth-block.component.scss']
})
export class AuthBlockComponent implements OnInit, OnDestroy {
  private route = inject(ActivatedRoute);
  private router = inject(Router);
  private auth = inject(AuthService);
  private idle = inject(IdleLogoutService);

  private destroy$ = new Subject<void>();

  type = this.route.snapshot.paramMap.get('type') ?? 'UNKNOWN';

  // ===== STATE =====
  isPolling = false;
  loading = false;
  attempt = 0;
  timedOut = false;

  config = computed(() => {
    if (this.type === 'DEVICE_PENDING' && this.timedOut) {
      return {
        title: 'Still Waiting for Approval',
        message:
          'This is taking longer than expected. Please contact your administrator or try again later.',
        action: 'Back to Login',
        retry: false
      };
    }

    switch (this.type) {
      case 'DEVICE_PENDING':
        return {
          title: 'Device Pending Approval',
          message: 'Waiting for device approval...',
          action: 'Retry',
          retry: true
        };
      case 'DEVICE_BLOCKED':
        return {
          title: 'Device Not Approved',
          message: 'This device is not approved for this branch.',
          action: 'Go Back',
          retry: false
        };
      case 'LOCATION_REQUIRED':
        return {
          title: 'Location Required',
          message: 'Location access is required to log in.',
          action: 'Retry',
          retry: true
        };
      case 'LOCATION_OUTSIDE':
        return {
          title: 'Outside Allowed Location',
          message: 'You are outside the permitted branch location.',
          action: 'Retry',
          retry: true
        };
      case 'LOCATION_ACCURACY':
        return {
          title: 'Low Location Accuracy',
          message: 'Your location accuracy is too low.',
          action: 'Retry',
          retry: true
        };
      case 'SESSION_LIMIT':
        return {
          title: 'Session Limit Reached',
          message: 'You have reached the maximum number of sessions.',
          action: 'Go Back',
          retry: false
        };
      default:
        return {
          title: 'Access Blocked',
          message: 'You cannot proceed at this time.',
          action: 'Go Back',
          retry: false
        };
    }
  });

  ngOnInit() {
    if (this.type === 'DEVICE_PENDING') {
      this.startPolling();
    }
  }

  // ===== POLLING =====
  private startPolling() {
    const payload = this.auth.getLastLoginRequest();
    if (!payload) {
      this.router.navigate(['/login']);
      return;
    }

    const startTime = Date.now();
    this.isPolling = true;

    const poll = () => {
      const elapsed = Date.now() - startTime;

      if (elapsed > 10 * 60 * 1000) {
        this.timedOut = true;
        this.stopPolling();
        return;
      }

      const delay = this.attempt < 3 ? 5000 : 10000;

      setTimeout(() => {
        this.attempt++;
        this.loading = true;

        this.auth.login(payload)
          .pipe(finalize(() => (this.loading = false)))
          .subscribe({
            next: (res) => {
              this.auth.clearLastLoginRequest();

              this.auth.setUser(res);
              this.idle.start();

              if (res.userType === 'PLATFORM') {
                this.router.navigateByUrl('/platform');
              } else {
                const landing = resolveTenantLanding(res.role);
                this.router.navigateByUrl(landing);
              }

              this.stopPolling();
            },
            error: (err) => {
              const msg = err?.error?.message ?? '';

              if (msg === 'Device pending approval') {
                poll(); // continue
                return;
              }

              if (msg === 'Device not approved for this branch') {
                this.stopPolling();
                this.router.navigate(['/auth/block/DEVICE_BLOCKED']);
                return;
              }

              this.stopPolling();
              this.auth.clearLastLoginRequest();
              this.router.navigate(['/login']);
            }
          });

      }, delay);
    };

    poll();
  }

  private stopPolling() {
    this.destroy$.next();
    this.destroy$.complete();
    this.isPolling = false;
  }

  // ===== ACTION =====
  onAction() {
    if (this.type === 'DEVICE_PENDING' && !this.timedOut) {
      this.attempt = 0;
      this.startPolling();
      return;
    }

    this.auth.clearLastLoginRequest();
    this.router.navigate(['/login'], { replaceUrl: true });
  }

  ngOnDestroy() {
    this.stopPolling();
  }
}