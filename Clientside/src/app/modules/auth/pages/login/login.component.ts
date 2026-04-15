import { Component, inject, OnInit } from '@angular/core';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { AuthService, LoginRequest } from '../../services/auth.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { finalize } from 'rxjs/operators';

import { CommonModule } from '@angular/common';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';
import { BranchService } from '../../../tenant/content/branches/services/branch.service';
import { DomainContextService } from '../../../../core/services/domain-context.service';
import { TenantBrandingService } from '../../../../core/services/tenant-branding.service';
import { resolveTenantLanding } from '../../../../core/utils/role-landing.util';
import { DeviceService } from '../../../../core/services/device.service';
import { LocationService } from '../../../../core/services/location.service';
import { WebAuthnService } from '../../../../core/services/webauthn.service';
import { BiometricRegistrationService } from '../../../../core/services/biometric-registration.service';
import { MatDialog } from '@angular/material/dialog';
import { BiometricPromptDialog } from '../../../../shared/components/biometric-prompt-dialog/biometric-prompt-dialog.component';

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
    MatProgressSpinnerModule,
    MatSnackBarModule
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

  form = this.fb.nonNullable.group({
    identifier: ['', Validators.required],
    password: ['', Validators.required],
    branchId: ['']
  });

  branches: Array<{ id: string; name: string }> = [];
  hide = true;
  loading = false;
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

      this.branchService.getAll(false).subscribe({
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
        error: () => {
          this.snack.open('Could not load branches', 'Close', { duration: 3500 });
        }
      });

    }
  }

  onForgot() {
    this.router.navigate(['/auth/forgot-password']);
  }

  async onBiometricLogin() {

    this.loading = true;

    let location;

    try {
      location = await this.locationService.getLocation();
    } catch {
      this.loading = false;
      this.snack.open(
        'Location access is required for biometric login.',
        'Close',
        { duration: 5000 }
      );
      return;
    }

    const deviceId = this.deviceService.getDeviceId();

    this.auth.biometricChallenge(deviceId).subscribe({
      next: async (challenge) => {

        try {
          const credential = await this.webauthn.authenticate(
            challenge.publicKeyCredentialRequestOptions
          );

          const payload = {
            rawJson: JSON.stringify(credential),
            branchId: this.domain.isPlatform
              ? null
              : this.form.value.branchId,
            deviceId,
            latitude: location.latitude,
            longitude: location.longitude,
            accuracy: location.accuracy
          };

          this.auth.biometricVerify(payload)
            .pipe(finalize(() => (this.loading = false)))
            .subscribe({
              next: (res) => {

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
                this.handleBiometricError(err);
              }
            });

        } catch (e) {
          this.loading = false;
          this.snack.open(
            'Biometric authentication cancelled or failed.',
            'Close',
            { duration: 4000 }
          );
        }

      },
      error: () => {
        this.loading = false;
        this.snack.open(
          'Unable to start biometric authentication.',
          'Close',
          { duration: 4000 }
        );
      }
    });
  }

  private handleBiometricError(err: any) {

    const msg: string = err?.error?.message ?? '';

    if (msg.includes('Device pending approval')) {
      this.snack.open('Device pending approval. Please wait for admin approval.', 'Close', { duration: 5000 });
      return;
    }

    if (msg.includes('Device not approved for this branch')) {
      this.snack.open('This device is not approved for this branch.', 'Close', { duration: 5000 });
      return;
    }

    if (msg.includes('Location required')) {
      this.snack.open('Location is required for biometric login.', 'Close', { duration: 5000 });
      return;
    }

    if (msg.includes('Outside branch location')) {
      this.snack.open('You are outside the allowed branch location.', 'Close', { duration: 5000 });
      return;
    }

    if (msg.includes('Location accuracy too low')) {
      this.snack.open('Location accuracy too low. Try again.', 'Close', { duration: 5000 });
      return;
    }

    if (msg.includes('Biometric authentication failed')) {
      this.snack.open('Biometric authentication failed.', 'Close', { duration: 4000 });
      return;
    }

    this.snack.open(msg || 'Biometric login failed', 'Close', { duration: 5000 });
  }

  async onSubmit() {

    if (this.form.invalid) {
      this.form.markAllAsTouched();
      return;
    }

    this.loading = true;

    let location;

    try {
      location = await this.locationService.getLocation();
    } catch {
      this.loading = false;

      this.snack.open(
        'Location access is required to log in. Please enable it in your browser settings.',
        'Close',
        { duration: 6000 }
      );

      return;
    }

    const payload: LoginRequest = {
      identifier: this.form.value.identifier!,
      password: this.form.value.password!,
      branchId: this.domain.isPlatform
        ? null as any
        : this.form.value.branchId!,
      deviceId: this.deviceService.getDeviceId(),
      latitude: location.latitude,
      longitude: location.longitude,
      accuracy: location.accuracy
    };

    this.auth.login(payload).pipe(
      finalize(() => (this.loading = false))
    ).subscribe({
      next: (res) => {
        console.log('LOGIN SUCCESS', res);

        this.auth.setUser(res);

        this.snack.open('Login successful', undefined, { duration: 1200 });

        this.idle.start();

        const deviceId = this.deviceService.getDeviceId();

        if (this.biometric.shouldPrompt(deviceId)) {

          this.biometric.markPromptShown(deviceId);

          const ref = this.dialog.open(BiometricPromptDialog);

          ref.afterClosed().subscribe((enable) => {
            if (enable === true) {
              this.biometric.register();
            }
          });
        }

        setTimeout(() => {
          if (res.userType === 'PLATFORM') {
            console.log('NAVIGATE PLATFORM');
            this.router.navigateByUrl('/platform');
          } else {
            const landing = resolveTenantLanding(res.role);
            console.log('NAVIGATE TENANT', landing);
            this.router.navigateByUrl(landing);
          }
        }, 0);
      },

      error: (err) => {

        const msg: string = err?.error?.message ?? '';

        if (msg.includes('Device pending approval')) {
          this.snack.open('Device pending approval. Please wait.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('Device not approved for this branch')) {
          this.snack.open('This device is not approved for this branch.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('Location required')) {
          this.snack.open('Location access is required.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('Outside branch location')) {
          this.snack.open('You are outside the allowed branch location.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('Location accuracy too low')) {
          this.snack.open('Location accuracy too low. Try again.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('Maximum active sessions reached for today') ||
          msg.includes('Too many active sessions')) {
          this.snack.open('Maximum active sessions reached. Please log out from another device.', 'Close', { duration: 5000 });
          return;
        }

        if (msg.includes('PASSWORD_CHANGE_REQUIRED')) {
          this.router.navigate(['/auth/reset-password'], {
            state: {
              forced: true,
              identifier: this.form.value.identifier
            }
          });
          return;
        }

        this.snack.open(
          msg || 'Invalid login credentials',
          'Close',
          { duration: 5000 }
        );
      }
    });
  }
}