import { Component, inject, OnInit } from '@angular/core';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { AuthService } from '../../services/auth.service';
import { ActivatedRoute, Router } from '@angular/router';
import { BranchService } from '../../../branches/services/branch.service';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { finalize } from 'rxjs/operators';

import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { LoginRequest } from '../../models';
import { IdleLogoutService } from '../../../../core/services/IdleLogoutService';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatCardModule,
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

  // form uses nonNullable to avoid null types
  form = this.fb.nonNullable.group({
    identifier: ['', Validators.required],
    password: ['', Validators.required],
    branchId: ['', Validators.required]
  });

  branches: Array<{ id: string; name: string }> = [];
  hide = true;
  loading = false;
  currentYear = new Date().getFullYear();

  ngOnInit() {

    // 🔒 Idle logout feedback
    if (this.route.snapshot.queryParamMap.get('reason') === 'idle') {
      this.snack.open(
        'You were logged out due to inactivity.',
        'Close',
        { duration: 4000 }
      );
    }

    this.branchService.getAll().subscribe({
      next: (data) => {
        this.branches = (Array.isArray(data) ? data : []).map((b: any) => ({
          id: b.id ?? '',
          name: b.name ?? ''
        }));
      },
      error: () => {
        this.snack.open('Could not load branches', 'Close', { duration: 3500 });
      }
    });
  }

  onForgot() {
    // navigate to forgot password UI or show dialog
    this.snack.open('Forgot password flow not implemented.', 'Close', { duration: 3500 });
  }

  onSubmit() {
    if (this.form.invalid) {
      this.form.markAllAsTouched();
      return;
    }

    this.loading = true;
    const payload = this.form.value as LoginRequest; // safe: nonNullable form

    this.auth.login(payload).pipe(
      finalize(() => (this.loading = false))
    ).subscribe({
      next: (res) => {
        this.auth.saveSession(res);
        this.snack.open('Login successful', undefined, { duration: 1200 });
        this.idle.start();
        this.router.navigate(['/dashboard']);
      },
      error: (err) => {
        // show useful message if available
        const msg = err?.error?.message || 'Invalid login credentials';
        this.snack.open(msg, 'Close', { duration: 5000 });
      }
    });
  }
}