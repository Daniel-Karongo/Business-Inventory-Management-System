import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Router } from '@angular/router';

import { PlanService } from '../../services/plan.service';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';

@Component({
  standalone: true,
  selector: 'app-plan-create',
  templateUrl: './plan-create.component.html',
  styleUrls: ['./plan-create.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatSnackBarModule,
    MatCheckboxModule
  ]
})
export class PlanCreateComponent {

  private fb = inject(FormBuilder);
  private planService = inject(PlanService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  loading = false;

  form = this.fb.nonNullable.group({

    code: ['', Validators.required],

    name: ['', Validators.required],

    maxUsers: [5, Validators.required],

    maxBranches: [1, Validators.required],

    inventoryEnabled: [true],

    accountingEnabled: [false],

    reportingEnabled: [false],

    requestsPerMinute: [300, Validators.required]

  });

  submit() {

    if (this.form.invalid) {

      this.form.markAllAsTouched();
      return;

    }

    this.loading = true;

    const payload = this.form.getRawValue();

    this.planService.createPlan(payload).subscribe({

      next: () => {

        this.snack.open(
          'Plan created successfully',
          'Close',
          { duration: 3000 }
        );

        this.router.navigate(['/platform/plans']);

      },

      error: () => {

        this.loading = false;

        this.snack.open(
          'Failed to create plan',
          'Close',
          { duration: 4000 }
        );

      }

    });

  }

  cancel() {

    this.router.navigate(['/platform/plans']);

  }

}