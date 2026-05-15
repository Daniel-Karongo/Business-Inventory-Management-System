import {
  Component,
  OnInit,
  inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  FormBuilder,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import {
  ActivatedRoute
} from '@angular/router';

import {
  finalize
} from 'rxjs';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatSlideToggleModule
} from '@angular/material/slide-toggle';

import {
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
  WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  BranchService
} from '../../services/branch.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-branch-email',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatSlideToggleModule,
    MatProgressSpinnerModule,
    WorkflowCardComponent
  ],
  templateUrl:
    './branch-email.component.html',
  styleUrls: [
    './branch-email.component.scss'
  ]
})
export class BranchEmailComponent
  implements OnInit {

  private fb = inject(FormBuilder);

  private route = inject(ActivatedRoute);

  private snackbar = inject(MatSnackBar);

  private branchService = inject(BranchService);

  branchId = '';

  loading = true;

  saving = false;

  form = this.fb.nonNullable.group({

    enabled: [false],

    host: ['', Validators.required],

    port: [587],

    username: [''],

    password: [''],

    fromAddress: [''],

    authEnabled: [true],

    tlsEnabled: [true],

    active: [true]
  });

  ngOnInit(): void {

    const id =
      this.route.parent?.snapshot.paramMap.get('id');

    if (!id) {
      return;
    }

    this.branchId = id;

    this.branchService
      .getEmailSettings(id)
      .subscribe({
        next: settings => {

          this.form.patchValue(settings);

          this.loading = false;
        },
        error: () => {
          this.loading = false;
        }
      });
  }

  save(): void {

    if (
      this.form.invalid ||
      this.saving
    ) {
      this.form.markAllAsTouched();
      return;
    }

    this.saving = true;

    this.branchService
      .updateEmailSettings(
        this.branchId,
        this.form.getRawValue()
      )
      .pipe(
        finalize(() => {
          this.saving = false;
        })
      )
      .subscribe({
        next: settings => {

          this.form.patchValue(settings);

          this.form.markAsPristine();

          this.snackbar.open(
            'Email settings updated',
            'Close',
            {
              duration: 3000
            }
          );
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to update email settings';

          this.snackbar.open(
            message,
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }
}