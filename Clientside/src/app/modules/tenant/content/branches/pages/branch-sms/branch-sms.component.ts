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
  selector: 'app-branch-sms',
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
    './branch-sms.component.html',
  styleUrls: [
    './branch-sms.component.scss'
  ]
})
export class BranchSmsComponent
  implements OnInit {

  private fb = inject(FormBuilder);

  private route = inject(ActivatedRoute);

  private branchService = inject(BranchService);

  private snackbar = inject(MatSnackBar);

  branchId = '';

  loading = true;

  saving = false;

  form = this.fb.nonNullable.group({

    enabled: [false],

    provider: ['AFRICASTALKING'],

    username: [''],

    apiKey: [''],

    senderId: [''],

    defaultCountryCode: ['+254'],

    sandbox: [true],

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
      .getSmsSettings(id)
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
      .updateSmsSettings(
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
            'SMS settings updated',
            'Close',
            {
              duration: 3000
            }
          );
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to update SMS settings';

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