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

  onSubmit() {
    if (this.form.invalid) {
      this.form.markAllAsTouched();
      return;
    }

    this.loading = true;
    const payload: LoginRequest = {
      identifier: this.form.value.identifier!,
      password: this.form.value.password!,
      branchId: this.domain.isPlatform
        ? null as any
        : this.form.value.branchId!
    };

    this.auth.login(payload).pipe(
      finalize(() => (this.loading = false))
    ).subscribe({
      next: (res) => {
        this.auth.setUser(res);
        this.snack.open('Login successful', undefined, { duration: 1200 });
        console.log('Starting idle service...');
        this.idle.start();
        console.log("Hello");
        console.log(res);
        if (res.userType === 'PLATFORM') {
          console.log("PLATFORM");
          this.router.navigateByUrl('/platform');
        } else {

          const landing = resolveTenantLanding(res.role);

          this.router.navigateByUrl(landing);

        }
      },
      error: (err) => {

        const msg = err?.error?.message ?? err?.error;

        if (msg === 'PASSWORD_CHANGE_REQUIRED') {

          this.router.navigate(
            ['/auth/reset-password'],
            {
              state: {
                forced: true,
                identifier: this.form.value.identifier
              }
            }
          );

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