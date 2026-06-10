import { CommonModule, CurrencyPipe } from '@angular/common';
import { Component, Inject, OnInit, inject } from '@angular/core';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar } from '@angular/material/snack-bar';
import { finalize } from 'rxjs';

import { OperationalExpenseService } from '../../services/operational-expense.service';
import { ApPaymentService } from '../../../ap/debts/services/ap-payment.service';
import { FundingAccount } from '../../../ap/debts/models/funding-account.model';

@Component({
    selector: 'app-bulk-settle-expenses-dialog',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        CurrencyPipe,
        MatDialogModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule
    ],
    templateUrl: './bulk-settle-expenses-dialog.component.html',
    styleUrls: ['./bulk-settle-expenses-dialog.component.scss']
})
export class BulkSettleExpensesDialogComponent implements OnInit {

    private readonly fb = inject(FormBuilder);
    private readonly service = inject(OperationalExpenseService);
    private readonly paymentService = inject(ApPaymentService);
    private readonly snack = inject(MatSnackBar);

    loading = false;
    saving = false;

    fundingAccounts: FundingAccount[] = [];

    readonly form = this.fb.nonNullable.group({
        fundingAccountId: ['', Validators.required],
        settlementDate: [
            new Date().toISOString().substring(0, 10),
            Validators.required
        ],
        reference: ['']
    });

    constructor(
        private readonly dialogRef: MatDialogRef<BulkSettleExpensesDialogComponent>,
        @Inject(MAT_DIALOG_DATA)
        public readonly data: {
            expenseIds: string[];
            expenseCount: number;
            totalOutstanding: number;
        }
    ) { }

    ngOnInit(): void {
        this.loadFundingAccounts();
    }

    get selectedAccount(): FundingAccount | undefined {
        return this.fundingAccounts.find(
            x => x.id === this.form.controls.fundingAccountId.value
        );
    }

    submit(): void {

        if (this.form.invalid || this.saving) {
            this.form.markAllAsTouched();
            return;
        }

        this.saving = true;

        this.service
            .bulkSettle({
                expenseIds: this.data.expenseIds,
                fundingAccountId: this.form.controls.fundingAccountId.value,
                settlementDate: this.form.controls.settlementDate.value,
                reference: this.form.controls.reference.value || undefined,
                sourceId: crypto.randomUUID()
            })
            .pipe(
                finalize(() => this.saving = false)
            )
            .subscribe({
                next: () => {
                    this.snack.open(
                        'Expenses settled successfully',
                        'Close',
                        { duration: 3500 }
                    );

                    this.dialogRef.close(true);
                },
                error: err => {
                    this.snack.open(
                        err?.error.message || 'Bulk settlement failed',
                        'Close',
                        { duration: 6000 }
                    );
                }
            });
    }

    private loadFundingAccounts(): void {

        this.loading = true;

        this.paymentService
            .fundingAccounts()
            .pipe(
                finalize(() => this.loading = false)
            )
            .subscribe({
                next: accounts => {

                    this.fundingAccounts = accounts;

                    if (!accounts.length) {
                        return;
                    }

                    const preferred =
                        accounts.find(a =>
                            (a.role || '')
                                .toUpperCase()
                                .includes('CASH')
                        )
                        ?? accounts.find(a =>
                            (a.role || '')
                                .toUpperCase()
                                .includes('BANK')
                        )
                        ?? accounts[0];

                    this.form.patchValue({
                        fundingAccountId: preferred.id
                    });
                },
                error: err => {
                    this.snack.open(
                        err?.error.message || 'Failed to load funding accounts',
                        'Close',
                        { duration: 6000 }
                    );
                }
            });
    }
}