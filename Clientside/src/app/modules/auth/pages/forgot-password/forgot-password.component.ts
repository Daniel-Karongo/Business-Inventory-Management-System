import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';

import { PasswordResetService, ResetChannel } from '../../services/password-reset.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-forgot-password',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule
  ],
  templateUrl: './forgot-password.component.html',
  styleUrls: ['./forgot-password.component.scss']
})
export class ForgotPasswordComponent implements OnInit {

  private fb = inject(FormBuilder);
  private reset = inject(PasswordResetService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  loading = false;
  channels: ResetChannel[] = [];

  form = this.fb.nonNullable.group({
    identifier: ['', Validators.required], // username only
    channel: ['IDENTITY' as ResetChannel, Validators.required]
  });

  ngOnInit() {
    this.reset.getOptions().subscribe(res => {
      this.channels = res.channels;
      if (!this.channels.includes(this.form.controls.channel.value)) {
        this.form.controls.channel.setValue(this.channels[0]);
      }
    });
  }

  submit() {
    if (this.form.invalid) return;

    const { identifier, channel } = this.form.value;

    this.loading = true;

    // Fire-and-forget backend call
    this.reset.initiate({
      identifier: identifier!,
      channel: channel!
    }).subscribe({
      complete: () => { },
      error: () => { }
    });

    // ✅ USER FEEDBACK (THIS IS THE PLACE)
    this.snack.open(
      channel === 'EMAIL'
        ? 'A verification code has been sent to your email.'
        : channel === 'SMS'
          ? 'A verification code has been sent to your phone.'
          : 'Proceed to identity verification.',
      'Close',
      { duration: 4000 }
    );

    this.loading = false;

    // Navigate immediately
    this.router.navigate(['/auth/reset-password'], {
      state: { identifier, channel }
    });
  }

  cancel() {
    this.router.navigate(['/login'], {
      replaceUrl: true,
      queryParams: { reason: 'idle' }
    });
  }
}