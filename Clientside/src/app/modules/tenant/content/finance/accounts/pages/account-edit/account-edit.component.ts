import {
  Component,
  OnInit,
  inject,
  signal
} from '@angular/core';

import { CommonModule }
  from '@angular/common';

import {
  FormBuilder,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';

import {
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  WorkflowCardComponent
} from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  AccountsApiAdapter
} from '../../adapters/accounts-api.adapter';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatSnackBarModule,
    WorkflowShellComponent,
    WorkflowCardComponent
  ],
  templateUrl:
    './account-edit.component.html',
  styleUrls: [
    './account-edit.component.scss'
  ]
})
export class AccountEditComponent
  implements OnInit {

  private readonly fb =
    inject(FormBuilder);

  private readonly api =
    inject(AccountsApiAdapter);

  private readonly route =
    inject(ActivatedRoute);

  private readonly router =
    inject(Router);

  private readonly snackbar =
    inject(MatSnackBar);

  readonly loading =
    signal(false);

  readonly saving =
    signal(false);

  accountId!: string;

  readonly form =
    this.fb.group({
      code: [
        {
          value: '',
          disabled: true
        }
      ],
      name: [
        '',
        Validators.required
      ],
      type: [
        {
          value: '',
          disabled: true
        }
      ]
    });

  ngOnInit(): void {

    const id =
      this.route
        .snapshot
        .paramMap
        .get('id');

    if (!id) {
      return;
    }

    this.accountId = id;

    this.load();
  }

  private load(): void {

    this.loading.set(true);

    this.api
      .get(this.accountId)
      .subscribe({
        next: account => {

          this.loading.set(false);

          this.form.patchValue({
            code: account.code,
            name: account.name,
            type: account.type
          });
        },
        error: () => {

          this.loading.set(false);

          this.snackbar.open(
            'Failed to load account',
            'Close',
            { duration: 3000 }
          );
        }
      });
  }

  submit(): void {

    if (
      this.form.invalid
      || this.saving()
    ) {
      return;
    }

    this.saving.set(true);

    this.api
      .rename(
        this.accountId,
        {
          name:
            this.form.value.name!
        }
      )
      .subscribe({
        next: () => {

          this.saving.set(false);

          this.snackbar.open(
            'Account updated successfully',
            'Close',
            { duration: 2500 }
          );
        },
        error: err => {

          this.saving.set(false);

          this.snackbar.open(
            err?.error?.message
            ||
            'Failed to update account',
            'Close',
            { duration: 3500 }
          );
        }
      });
  }

  cancel(): void {

    this.router.navigate([
      '/app/finance/accounting/chart'
    ]);
  }

}