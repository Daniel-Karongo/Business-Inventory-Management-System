import { Component, inject, OnInit, OnDestroy } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { PasswordResetService, ResetChannel } from '../../services/password-reset.service';

@Component({
  standalone: true,
  selector: 'app-reset-password',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './reset-password.component.html',
  styleUrls: ['./reset-password.component.scss']
})
export class ResetPasswordComponent implements OnInit, OnDestroy {

  private fb = inject(FormBuilder);
  private reset = inject(PasswordResetService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  state = history.state as {
    identifier: string;
    channel: ResetChannel;
  };

  /* =========================
     UI STATE
     ========================= */
  step: 'VERIFY' | 'RESET' = 'VERIFY';
  loading = false;
  verifyDisabled = false;

  hidePw = true;
  hideConfirm = true;

  /* =========================
     OTP TIMER
     ========================= */
  otpExpirySeconds = 15 * 60; // 15 minutes (email)
  remainingSeconds = this.otpExpirySeconds;
  timerId?: number;

  /* =========================
     FORM
     ========================= */
  form = this.fb.nonNullable.group({
    token: ['', Validators.required],
    newPassword: [''],
    confirmPassword: ['']
  }, {
    validators: form => {
      const pw = form.get('newPassword')!.value;
      const cpw = form.get('confirmPassword')!.value;
      return pw && cpw && pw !== cpw ? { mismatch: true } : null;
    }
  });

  /* =========================
     LIFECYCLE
     ========================= */
  ngOnInit() {
    this.startCountdown();
  }

  ngOnDestroy() {
    this.clearTimer();
  }

  /* =========================
     OTP VERIFICATION (UI-ONLY)
     ========================= */
  verifyCode() {
    if (this.form.controls.token.invalid || this.verifyDisabled) return;

    this.verifyDisabled = true;

    this.reset.verifyToken({ token: this.form.value.token! })
      .subscribe({
        next: () => {
          this.step = 'RESET';

          this.form.controls.newPassword.setValidators([Validators.required]);
          this.form.controls.confirmPassword.setValidators([Validators.required]);

          this.form.controls.newPassword.updateValueAndValidity();
          this.form.controls.confirmPassword.updateValueAndValidity();

          this.snack.open(
            'Code verified. Please set a new password.',
            'Close',
            { duration: 3000 }
          );
        },
        error: () => {
          this.verifyDisabled = false;

          this.snack.open(
            'Invalid or expired verification code.',
            'Close',
            { duration: 3500 }
          );
        }
      });
  }

  /* =========================
     RESEND OTP
     ========================= */
  resendCode() {
    this.reset.initiate({
      identifier: this.state.identifier,
      channel: this.state.channel
    }).subscribe({
      complete: () => { },
      error: () => { }
    });

    this.remainingSeconds = this.otpExpirySeconds;
    this.startCountdown();

    this.verifyDisabled = false;
    this.form.controls.token.reset();
    this.step = 'VERIFY';

    this.snack.open(
      this.state.channel === 'EMAIL'
        ? 'A new verification code has been sent to your email.'
        : 'A new verification code has been sent to your phone.',
      'Close',
      { duration: 4000 }
    );
  }

  /* =========================
     FINAL RESET
     ========================= */
  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    const payload = {
      token: this.form.value.token!,
      newPassword: this.form.value.newPassword!
    };

    this.reset.completeWithToken(payload).subscribe({
      next: () => {
        this.snack.open(
          'Password reset successful. You can now log in.',
          'Close',
          { duration: 3000 }
        );
        this.finish();
      },
      error: () => {
        this.snack.open(
          'Password reset failed. Please try again.',
          'Close',
          { duration: 3500 }
        );
        this.loading = false;
      }
    });
  }

  /* =========================
     TIMER HELPERS
     ========================= */
  private startCountdown() {
    this.clearTimer();

    this.timerId = window.setInterval(() => {
      if (this.remainingSeconds > 0) {
        this.remainingSeconds--;
      } else {
        this.clearTimer();
      }
    }, 1000);
  }

  private clearTimer() {
    if (this.timerId) {
      clearInterval(this.timerId);
      this.timerId = undefined;
    }
  }

  get countdownLabel(): string {
    const min = Math.floor(this.remainingSeconds / 60);
    const sec = this.remainingSeconds % 60;
    return `${min}:${sec.toString().padStart(2, '0')}`;
  }

  get maskedDestination(): string {
    if (this.state.channel === 'EMAIL') {
      return this.maskEmail(this.state.identifier);
    }

    if (this.state.channel === 'SMS') {
      return this.maskPhone(this.state.identifier);
    }

    return 'your registered account details';
  }

  private maskEmail(value: string): string {
    if (!value || !value.includes('@')) {
      return 'your registered email';
    }

    const [name, domain] = value.split('@');
    if (name.length <= 1) {
      return `*@${domain}`;
    }

    return `${name.charAt(0)}***@${domain}`;
  }

  private maskPhone(value: string): string {
    const digits = value.replace(/\D/g, '');

    if (digits.length < 7) {
      return 'your registered phone';
    }

    return `+${digits.slice(0, 3)}***${digits.slice(-3)}`;
  }

  /* =========================
     NAVIGATION
     ========================= */
  private finish() {
    this.clearTimer();
    this.loading = false;
    this.router.navigate(['/login'], {
      replaceUrl: true,
      queryParams: { reason: 'idle' }
    });
  }

  cancel() {
    this.finish();
  }
}