import {
  Component,
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

import { Router }
  from '@angular/router';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatSelectModule
} from '@angular/material/select';

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
import { AccountRole, AccountType } from '../../models/account.models';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatSnackBarModule,
    WorkflowShellComponent,
    WorkflowCardComponent
  ],
  templateUrl:
    './account-create.component.html',
  styleUrls: [
    './account-create.component.scss'
  ]
})
export class AccountCreateComponent {

  private readonly fb =
    inject(FormBuilder);

  private readonly api =
    inject(AccountsApiAdapter);

  private readonly snackbar =
    inject(MatSnackBar);

  private readonly router =
    inject(Router);

  readonly saving =
    signal(false);

  readonly form =
    this.fb.nonNullable.group({
      code: [
        '',
        Validators.required
      ],

      name: [
        '',
        Validators.required
      ],

      type: [
        'ASSET' as AccountType,
        Validators.required
      ],

      role: [
        '',
        Validators.required
      ]
    });
  readonly accountTypes: AccountType[] = [
    'ASSET',
    'LIABILITY',
    'EQUITY',
    'INCOME',
    'EXPENSE'
  ];

  constructor() {
    this.form.controls.name.valueChanges
      .subscribe(name => {

        this.form.patchValue(
          {
            role: this.generateRole(name ?? '')
          },
          {
            emitEvent: false
          }
        );

      });
  }

  private generateRole(name: string): string {
    return name
      .trim()
      .toUpperCase()
      .replace(/[^A-Z0-9]+/g, '_')
      .replace(/^_+|_+$/g, '');
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
      .create({
        code:
          this.form.value.code!,
        name:
          this.form.value.name!,
        type:
          this.form.getRawValue().type,
        role:
          this.form.getRawValue().role
      })
      .subscribe({
        next: account => {

          this.saving.set(false);

          this.snackbar.open(
            'Account created successfully',
            'Close',
            { duration: 2500 }
          );

          this.router.navigate([
            '/app/finance/accounting/chart',
            account.id
          ]);
        },
        error: err => {

          this.saving.set(false);

          this.snackbar.open(
            err?.error?.message
            ||
            'Failed to create account',
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