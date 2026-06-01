import {
  Component,
  OnInit,
  inject
} from '@angular/core';

import { CommonModule }
  from '@angular/common';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatDialog,
  MatDialogModule
} from '@angular/material/dialog';

import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';

import {
  MatTableModule
} from '@angular/material/table';

import {
  finalize
} from 'rxjs/operators';

import {
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  WorkflowCardComponent
} from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  JournalService
} from '../../services/journal.service';

import {
  Journal
} from '../../models/journal.models';

import {
  ReasonDialogComponent
} from '../../../../../../../shared/components/reason-dialog/reason-dialog.component';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatDialogModule,
    MatSnackBarModule,
    MatTableModule,
    WorkflowShellComponent,
    WorkflowCardComponent
  ],
  templateUrl:
    './journal-details.component.html',
  styleUrls: [
    './journal-details.component.scss'
  ]
})
export class JournalDetailsComponent
  implements OnInit {

  private readonly route =
    inject(ActivatedRoute);

  private readonly router =
    inject(Router);

  private readonly journalService =
    inject(JournalService);

  private readonly dialog =
    inject(MatDialog);

  private readonly snackbar =
    inject(MatSnackBar);

  journal?: Journal;

  loading = false;

  reversing = false;

  readonly displayedColumns = [
    'account',
    'direction',
    'amount'
  ];

  ngOnInit(): void {

    const id =
      this.route
        .snapshot
        .paramMap
        .get('id');

    if (!id) {
      return;
    }

    this.load(id);
  }

  load(
    id: string
  ): void {

    this.loading = true;

    this.journalService
      .get(id)
      .pipe(
        finalize(() => {
          this.loading = false;
        })
      )
      .subscribe({
        next: journal => {
          this.journal = journal;
        },
        error: () => {
          this.snackbar.open(
            'Failed to load journal',
            'Close',
            { duration: 3000 }
          );
        }
      });
  }

  reverse(): void {

    if (
      !this.journal
      || this.reversing
    ) {
      return;
    }

    const ref =
      this.dialog.open(
        ReasonDialogComponent,
        {
          width: '420px',
          panelClass: 'enterprise-dialog',
          data: {
            title:
              'Reverse Transaction?',

            message:
              'This will create a reversing accounting entry. Select the reason for reversal.',

            action:
              'REVERSE',

            confirmText:
              'Reverse Transaction',

            requireReason: true,

            allowCustomReason: true,

            reasons: [
              'Incorrect account selected',
              'Incorrect amount entered',
              'Duplicate transaction',
              'Wrong accounting period',
              'Transaction posted by mistake',
              'Correction of previous entry'
            ]
          }
        }
      );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.reversing = true;

        this.journalService
          .reverse(
            this.journal!.id,
            {
              reason:
                result.reason
            }
          )
          .pipe(
            finalize(() => {
              this.reversing = false;
            })
          )
          .subscribe({
            next: () => {

              this.snackbar.open(
                'Transaction reversed successfully',
                'Close',
                { duration: 2500 }
              );

              this.load(
                this.journal!.id
              );
            },
            error: err => {

              this.snackbar.open(
                this.buildFriendlyReverseError(
                  err
                ),
                'Close',
                {
                  duration: 7000
                }
              );
            }
          });
      });
  }

  back(): void {

    this.router.navigate([
      '/app/finance/accounting/journals'
    ]);
  }

  total(): number {

    if (
      !this.journal?.entries?.length
    ) {
      return 0;
    }

    return this.journal.entries
      .reduce(
        (sum, e) =>
          sum + e.amount,
        0
      );
  }

  private buildFriendlyReverseError(
    err: any
  ): string {

    const message =
      err?.error?.message
      || '';

    if (
      message.includes(
        'immutable'
      )
    ) {
      return `
This transaction could not be reversed.

The accounting system prevented changes to a locked journal entry.

Please contact your administrator if this transaction should still be reversible.
    `.trim();
    }

    if (
      message.includes(
        'already reversed'
      )
    ) {
      return `
This transaction has already been reversed.
    `.trim();
    }

    return (
      err?.error?.message
      || 'Failed to reverse transaction'
    );
  }
}