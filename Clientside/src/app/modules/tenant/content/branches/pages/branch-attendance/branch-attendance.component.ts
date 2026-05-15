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
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
  WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  BranchService
} from '../../services/branch.service';

import {
  BranchAttendanceSettingsDTO
} from '../../models/branch.model';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-branch-attendance',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatProgressSpinnerModule,
    WorkflowCardComponent
  ],
  templateUrl:
    './branch-attendance.component.html',
  styleUrls: [
    './branch-attendance.component.scss'
  ]
})
export class BranchAttendanceComponent
  implements OnInit {

  private fb = inject(FormBuilder);

  private route = inject(ActivatedRoute);

  private snackbar = inject(MatSnackBar);

  private branchService = inject(BranchService);

  loading = true;

  saving = false;

  branchId = '';

  form = this.fb.nonNullable.group({

    rollcallStartTime: [
      ''
    ],

    rollcallGraceMinutes: [
      0,
      [
        Validators.min(0)
      ]
    ],

    logoutTime: [
      ''
    ]
  });

  ngOnInit(): void {

    const id =
      this.route.parent?.snapshot.paramMap.get('id');

    if (!id) {
      return;
    }

    this.branchId = id;

    this.branchService
      .getById(id)
      .subscribe({
        next: branch => {

          this.form.patchValue({
            rollcallStartTime:
              branch.rollcallStartTime || '',
            rollcallGraceMinutes:
              branch.rollcallGraceMinutes || 0,
            logoutTime:
              branch.logoutTime || ''
          });

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

    const payload:
      BranchAttendanceSettingsDTO = {

      rollcallStartTime:
        this.form.getRawValue()
          .rollcallStartTime || null,

      rollcallGraceMinutes:
        this.form.getRawValue()
          .rollcallGraceMinutes,

      logoutTime:
        this.form.getRawValue()
          .logoutTime || null
    };

    this.branchService
      .updateAttendanceSettings(
        this.branchId,
        payload
      )
      .pipe(
        finalize(() => {
          this.saving = false;
        })
      )
      .subscribe({
        next: branch => {

          this.form.patchValue({
            rollcallStartTime:
              branch.rollcallStartTime || '',
            rollcallGraceMinutes:
              branch.rollcallGraceMinutes || 0,
            logoutTime:
              branch.logoutTime || ''
          });

          this.form.markAsPristine();

          this.snackbar.open(
            'Attendance settings updated',
            'Close',
            {
              duration: 3000
            }
          );
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to update attendance settings';

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