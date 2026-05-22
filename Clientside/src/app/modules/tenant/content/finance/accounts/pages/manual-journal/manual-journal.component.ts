import {
  Component,
  OnInit,
  computed,
  inject,
  signal
} from '@angular/core';

import { CommonModule }
  from '@angular/common';

import {
  FormArray,
  FormBuilder,
  FormGroup,
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
  MatIconModule
} from '@angular/material/icon';

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

import { AccountsService }
  from '../../services/accounts.service';

import {
  ManualJournalService
} from '../../services/manual-journal.service';

import {
  Account
} from '../../models/account.models';

import {
  ManualJournalRequest
} from '../../models/journal.models';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule,
    WorkflowShellComponent,
    WorkflowCardComponent
  ],
  templateUrl:
    './manual-journal.component.html',
  styleUrls: [
    './manual-journal.component.scss'
  ]
})
export class ManualJournalComponent
  implements OnInit {

  private readonly fb =
    inject(FormBuilder);

  private readonly accountsService =
    inject(AccountsService);

  private readonly journalService =
    inject(ManualJournalService);

  private readonly snackbar =
    inject(MatSnackBar);

  private readonly router =
    inject(Router);

  private readonly branchContext =
    inject(BranchContextService);

  readonly loading =
    signal(false);

  readonly posting =
    signal(false);

  readonly accounts =
    signal<Account[]>([]);

  form!: FormGroup;

  readonly debitTotal =
    computed(() => {

      return this.lines.controls
        .filter(
          g =>
            g.value.direction
            === 'DEBIT'
        )
        .reduce(
          (sum, g) =>
            sum + (
              Number(
                g.value.amount
              ) || 0
            ),
          0
        );
    });

  readonly creditTotal =
    computed(() => {

      return this.lines.controls
        .filter(
          g =>
            g.value.direction
            === 'CREDIT'
        )
        .reduce(
          (sum, g) =>
            sum + (
              Number(
                g.value.amount
              ) || 0
            ),
          0
        );
    });

  readonly balanced =
    computed(() =>
      this.debitTotal()
      === this.creditTotal()
    );

  ngOnInit(): void {

    this.initializeForm();

    this.loadAccounts();
  }

  private initializeForm(): void {

    this.form = this.fb.group({
      reference: [
        '',
        Validators.required
      ],
      description: [''],
      lines: this.fb.array([])
    });

    this.addLine();

    this.addLine();
  }

  private loadAccounts(): void {

    this.loading.set(true);

    this.accountsService
      .list({
        page: 0,
        size: 500,
        sort: 'code'
      })
      .subscribe({
        next: res => {

          this.accounts.set(
            res.content ?? []
          );

          this.loading.set(false);
        },
        error: () => {

          this.loading.set(false);

          this.snackbar.open(
            'Failed to load accounts',
            'Close',
            { duration: 3000 }
          );
        }
      });
  }

  private resolveBranchId(): string {

    const branchId =
      this.branchContext.currentBranch;

    if (!branchId) {

      throw new Error(
        'Branch not selected'
      );
    }

    return branchId;
  }

  get lines(): FormArray {

    return this.form.get(
      'lines'
    ) as FormArray;
  }

  addLine(): void {

    this.lines.push(
      this.fb.group({
        accountId: [
          '',
          Validators.required
        ],
        direction: [
          'DEBIT',
          Validators.required
        ],
        amount: [
          null,
          [
            Validators.required,
            Validators.min(0.01)
          ]
        ]
      })
    );
  }

  removeLine(
    index: number
  ): void {

    if (this.lines.length <= 2) {

      this.snackbar.open(
        'At least two entries are required',
        'Close',
        { duration: 2500 }
      );

      return;
    }

    this.lines.removeAt(index);
  }

  submit(): void {

    if (
      this.form.invalid
      || this.posting()
    ) {
      return;
    }

    if (!this.balanced()) {

      this.snackbar.open(
        'Entries are not balanced',
        'Close',
        { duration: 3000 }
      );

      return;
    }

    this.posting.set(true);

    const payload:
      ManualJournalRequest = {

      reference:
        this.form.value.reference,

      description:
        this.form.value.description,

      branchId:
        this.resolveBranchId(),

      accountingDate:
        new Date()
          .toISOString()
          .split('T')[0],

      lines:
        this.form.value.lines
    };

    this.journalService
      .post(payload)
      .subscribe({
        next: () => {

          this.posting.set(false);

          this.snackbar.open(
            'Transaction posted successfully',
            'Close',
            { duration: 2500 }
          );

          this.router.navigate([
            '/app/finance/accounting/journals'
          ]);
        },
        error: err => {

          this.posting.set(false);

          this.snackbar.open(
            err?.error?.message
            ||
            'Failed to post transaction',
            'Close',
            { duration: 3500 }
          );
        }
      });
  }

  cancel(): void {

    this.router.navigate([
      '/app/finance/accounting/journals'
    ]);
  }

}