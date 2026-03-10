import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { TenantService } from '../../services/tenant.service';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-tenant-create',
  templateUrl: './tenant-create.component.html',
  styleUrls: ['./tenant-create.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatSnackBarModule
  ]
})
export class TenantCreateComponent {

  private fb = inject(FormBuilder);
  private tenantService = inject(TenantService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  loading = false;

  form = this.fb.nonNullable.group({
    name: ['', Validators.required],
    code: ['', Validators.required],
    adminUsername: ['admin'],
    adminPassword: ['ChangeMe123!']
  });

  submit() {

    if (this.form.invalid) {
      this.form.markAllAsTouched();
      return;
    }

    this.loading = true;

    const payload = this.form.getRawValue();

    this.tenantService.createTenant(payload).subscribe({

      next: () => {

        this.snack.open(
          'Tenant created successfully',
          'Close',
          { duration: 3000 }
        );

        this.router.navigate(['/platform/tenants']);

      },

      error: () => {

        this.loading = false;

        this.snack.open(
          'Tenant creation failed',
          'Close',
          { duration: 4000 }
        );

      }

    });

  }

  cancel() {
    this.router.navigate(['/platform/tenants']);
  }

}