import { Component, inject, OnInit } from '@angular/core';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { ActivatedRoute, Router } from '@angular/router';
import { finalize } from 'rxjs/operators';
import { AuthService, LoginRequest } from '../../services/auth.service';

import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { AuthErrorService } from '../../../../core/services/auth-error.service';
import { BiometricRegistrationService } from '../../../../core/services/biometric-registration.service';
import { DeviceService } from '../../../../core/services/device.service';
import { DomainContextService } from '../../../../core/services/domain-context.service';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';
import { LocationService } from '../../../../core/services/location.service';
import { TenantBrandingService } from '../../../../core/services/tenant-branding.service';
import { WebAuthnService } from '../../../../core/services/webauthn.service';
import { resolveTenantLanding } from '../../../../core/utils/role-landing.util';
import { BiometricPromptDialog } from '../../../../shared/components/biometric-prompt-dialog/biometric-prompt-dialog.component';
import { BranchService } from '../../../tenant/content/branches/services/branch.service';
import { EnterNextDirective } from '../../../../shared/directives/enter-next.directive';

import {
  SessionService
} from '../../../../core/services/session.service';

import { SessionLimitInfoResponse } from '../../../../core/models/session.models';

import { SessionRecoveryDialogComponent } from '../../dialogs/session-recovery-dialog/session-recovery-dialog.component';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule,
    EnterNextDirective,
    MatTooltipModule
  ],
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {

  private fb = inject(FormBuilder);
  private auth = inject(AuthService);
  private router = inject(Router);
  private branchService = inject(BranchService);
  private snack = inject(MatSnackBar);
  private route = inject(ActivatedRoute);
  private idle = inject(IdleLogoutService);
  private domain = inject(DomainContextService);
  private branding = inject(TenantBrandingService);
  private deviceService = inject(DeviceService);
  private locationService = inject(LocationService);
  private webauthn = inject(WebAuthnService);
  private biometric = inject(BiometricRegistrationService);
  private dialog = inject(MatDialog);
  private errorHandler = inject(AuthErrorService);
  private sessionService = inject(SessionService);

  form = this.fb.nonNullable.group({
    identifier: ['', Validators.required],
    password: ['', Validators.required],
    branchId: ['']
  });

  branches: Array<{ id: string; name: string }> = [];
  hide = true;
  loginLoading = false;
  biometricLoading = false;
  currentYear = new Date().getFullYear();
  isPlatform = this.domain.isPlatform;
  logo$ = this.branding.logo$;

  ngOnInit() {

    if (this.route.snapshot.queryParamMap.get('reason') === 'idle') {
      this.snack.open(
        'You were logged out due to inactivity.',
        'Close',
        { duration: 4000 }
      );
    }

    if (!this.isPlatform) {

      this.form.controls.branchId.addValidators(Validators.required);

      this.branchService.getAllLegacy().subscribe({
        next: (data) => {

          const list = (Array.isArray(data) ? data : [])
            .map((b: any) => ({
              id: b.id ?? '',
              name: b.name ?? ''
            }));

          this.branches = list;

          if (list.length === 1) {
            this.form.patchValue({ branchId: list[0].id });
          }

          if (list.length === 0) {
            this.snack.open('No branches configured for this tenant.', 'Close', { duration: 4000 });
          }

        },
        error: (err) => {

          if (err.status === 0) {
            return;
          }

          if (err.status === 500 && !err.error) {
            this.snack.open(
              'Server is unavailable. Please try again later.',
              'Close',
              { duration: 4000 }
            );
            return;
          }

          this.snack.open('Could not load branches', 'Close', { duration: 3500 });
        }
      });

    }
  }

  onForgot() {
    this.router.navigate(['/auth/forgot-password']);
  }

  async onBiometricLogin() {

    if (!this.domain.isPlatform && !this.form.value.branchId) {
      this.snack.open(
        'Select a branch before biometric login.',
        'Close',
        { duration: 4000 }
      );
      return;
    }

    this.biometricLoading = true;

    let location;

    try {
      location = await this.locationService.getLocation();
    } catch {
      location = {
        latitude: 0,
        longitude: 0,
        accuracy: 0
      };
    }

    const deviceId = this.deviceService.getDeviceId();

    this.auth.biometricChallenge(
      deviceId,
      this.form.value.branchId ?? null
    ).subscribe({
      next: async (challenge) => {

        try {
          const credential = await this.webauthn.authenticate(
            challenge
          );

          const ua = navigator.userAgent;

          const payload = {
            credential: { ...credential },

            branchId: this.domain.isPlatform
              ? null
              : this.form.value.branchId,

            deviceId,

            latitude: location.latitude,
            longitude: location.longitude,
            accuracy: location.accuracy,

            userAgent: ua,
            browserName: this.detectBrowser(ua),
            osName: this.detectOS(ua),
            platform: navigator.platform
          };

          this.auth.biometricVerify(payload)
            .pipe(finalize(() => (this.biometricLoading = false)))
            .subscribe({
              next: (res) => {
                this.biometricLoading = false;
                this.auth.setUser(res);
                this.idle.start();

                if (res.userType === 'PLATFORM') {
                  this.router.navigateByUrl('/platform');
                } else {
                  const landing = resolveTenantLanding(res.role);
                  this.router.navigateByUrl(landing);
                }
              },
              error: (err) => {
                this.errorHandler.handle(err, 'biometric');
              }
            });

        } catch (e) {
          this.biometricLoading = false;
          this.snack.open(
            'Biometric authentication cancelled or failed.',
            'Close',
            { duration: 6000 }
          );
        }

      },
      error: (err) => {
        this.errorHandler.handle(err, 'biometric');
        this.biometricLoading = false;
      }
    });
  }

  async onSubmit() {
    if (this.form.invalid) {
      this.form.markAllAsTouched();
      return;
    }

    this.loginLoading = true;

    let location;

    try {
      location = await this.locationService.getLocation();
    } catch {
      location = {
        latitude: 0,
        longitude: 0,
        accuracy: 0
      };
    }

    const ua = navigator.userAgent;

    const payload: LoginRequest = {
      identifier: this.form.value.identifier!,
      password: this.form.value.password!,
      branchId: this.domain.isPlatform
        ? null
        : this.form.value.branchId!,
      deviceId: this.deviceService.getDeviceId(),

      latitude: location.latitude,
      longitude: location.longitude,
      accuracy: location.accuracy,

      userAgent: ua,

      browserName: this.detectBrowser(ua),
      osName: this.detectOS(ua),
      platform: navigator.platform
    };

    this.auth.login(payload).pipe(
      finalize(() => (this.loginLoading = false))
    ).subscribe({
      next: (res) => {
        this.loginLoading = false;

        this.auth.setUser(res);

        this.snack.open('Login successful', undefined, { duration: 1200 });

        this.idle.start();

        const deviceId = this.deviceService.getDeviceId();

        if (this.biometric.shouldPrompt(deviceId)) {

          const ref = this.dialog.open(BiometricPromptDialog, {
            width: '440px',
            maxWidth: '95vw',
            maxHeight: '90vh',
            autoFocus: false,
            panelClass: 'enterprise-dialog'
          });

          ref.afterClosed().subscribe(enable => {
            if (enable === true) {
              this.biometric.markPromptShown(deviceId);
              this.biometric.register();
            }
          });
        }

        setTimeout(() => {
          if (res.userType === 'PLATFORM') {
            this.router.navigateByUrl('/platform');
          } else {
            const landing = resolveTenantLanding(res.role);
            this.router.navigateByUrl(landing);
          }
        }, 0);
      },

      error: (err) => {

        const code =
          err?.error?.code;

        if (
          code ===
          'DEVICE_LIMIT_REACHED'
        ) {

          this.handleDeviceLimitReached(
            payload
          );

          this.loginLoading = false;

          return;
        }

        this.errorHandler.handle(
          err,
          'login',
          this.form.value.identifier!
        );

        this.loginLoading = false;
      }
    });
  }

  private detectBrowser(ua: string): string {

    if (ua.includes('Edg')) return 'Edge';
    if (ua.includes('Chrome')) return 'Chrome';
    if (ua.includes('Firefox')) return 'Firefox';
    if (ua.includes('Safari')) return 'Safari';

    return 'Unknown';
  }

  private detectOS(ua: string): string {

    if (ua.includes('Windows')) return 'Windows';
    if (ua.includes('Android')) return 'Android';
    if (ua.includes('Mac OS')) return 'macOS';
    if (ua.includes('Linux')) return 'Linux';

    return 'Unknown';
  }

  private handleDeviceLimitReached(
    payload: LoginRequest
  ) {

    this.sessionService
      .getSessionLimitInfo({
        identifier: payload.identifier,
        branchId: payload.branchId,
        deviceId: payload.deviceId
      })
      .subscribe({

        next: (
          info: SessionLimitInfoResponse
        ) => {

          const ref =
            this.dialog.open(
              SessionRecoveryDialogComponent,
              {
                width: '1000px',
                maxWidth: '95vw',
                panelClass: 'enterprise-dialog',
                disableClose: true,
                data: {
                  identifier: payload.identifier,
                  password: payload.password,
                  branchId: payload.branchId,
                  sessions: info.sessions
                }
              }
            );

          ref.afterClosed()
            .subscribe(result => {

              if (result === true) {

                this.auth.login(payload)
                  .subscribe({

                    next: (res) => {

                      this.auth.setUser(res);

                      this.idle.start();

                      if (
                        res.userType === 'PLATFORM'
                      ) {

                        this.router.navigateByUrl(
                          '/platform'
                        );

                      } else {

                        const landing =
                          resolveTenantLanding(
                            res.role
                          );

                        this.router.navigateByUrl(
                          landing
                        );

                      }
                    }
                  });
              }
            });
        }
      });
  }

  get loginDisabledReason(): string {
    if (!this.form.controls.identifier.value?.trim()) {
      return 'Enter your username';
    }

    if (!this.form.controls.password.value?.trim()) {
      return 'Enter your password';
    }

    if (
      !this.isPlatform &&
      this.branches.length > 1 &&
      !this.form.controls.branchId.value
    ) {
      return 'Select a branch';
    }

    return '';
  }
}