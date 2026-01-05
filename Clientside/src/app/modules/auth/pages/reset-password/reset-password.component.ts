import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

import { PasswordResetService, ResetChannel } from '../../services/password-reset.service';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

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
export class ResetPasswordComponent {

  private fb = inject(FormBuilder);
  private reset = inject(PasswordResetService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  state = history.state as {
    identifier: string;
    channel: ResetChannel;
  };

  loading = false;
  hidePw = true;
  hideConfirm = true;

  form = this.fb.nonNullable.group({
    token: [''],
    email: [''],
    idNumber: [''],
    newPassword: ['', Validators.required],
    confirmPassword: ['', Validators.required]
  }, {
    validators: form => {
      const pw = form.get('newPassword')!.value;
      const cpw = form.get('confirmPassword')!.value;
      return pw && cpw && pw !== cpw ? { mismatch: true } : null;
    }
  });

  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    const payload =
      this.state.channel === 'IDENTITY'
        ? {
          identifier: this.state.identifier,
          email: this.form.value.email || undefined,
          idNumber: this.form.value.idNumber || undefined,
          newPassword: this.form.value.newPassword!
        }
        : {
          token: this.form.value.token!,
          newPassword: this.form.value.newPassword!
        };

    this.reset.completeWithIdentity(payload as any)
      .subscribe({
        next: () => {
          this.snack.open(
            'Password reset successful. You can now log in with your new password.',
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
          this.finish();
        }
      });
  }

  private finish() {
    this.loading = false;
    this.router.navigate(['/login']);
  }

  cancel() {
    this.router.navigate(['/login']);
  }
}