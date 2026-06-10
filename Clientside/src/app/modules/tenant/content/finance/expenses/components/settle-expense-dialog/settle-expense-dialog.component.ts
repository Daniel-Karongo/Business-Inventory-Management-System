import { CommonModule, CurrencyPipe } from '@angular/common';
import { Component, Inject, OnInit, inject } from '@angular/core';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { finalize } from 'rxjs';

import { OperationalExpenseService } from '../../services/operational-expense.service';
import { ApPaymentService } from '../../../ap/debts/services/ap-payment.service';
import { FundingAccount } from '../../../ap/debts/models/funding-account.model';

@Component({
    selector: 'app-settle-expense-dialog',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        CurrencyPipe,
        MatDialogModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatDatepickerModule,
        MatIconModule
    ],
    templateUrl: './settle-expense-dialog.component.html',
    styleUrls: ['./settle-expense-dialog.component.scss']
})
export class SettleExpenseDialogComponent implements OnInit {

    private readonly fb = inject(FormBuilder);
    private readonly service = inject(OperationalExpenseService);
    private readonly apPaymentService = inject(ApPaymentService);
    private readonly snack = inject(MatSnackBar);

    loading = false;
    saving = false;

    fundingAccounts: FundingAccount[] = [];

    readonly form = this.fb.nonNullable.group({
        fundingAccountId: ['', Validators.required],
        amount: [0, [Validators.required, Validators.min(0.01)]],
        settlementDate: [new Date().toISOString().substring(0, 10), Validators.required],
        reference: ['']
    });

    constructor(
        private readonly dialogRef: MatDialogRef<SettleExpenseDialogComponent>,
        @Inject(MAT_DIALOG_DATA)
        public readonly data: {
            expenseId: string;
            description: string;
            amount: number;
            settledAmount: number;
            outstandingAmount: number
        }
    ) { }

    ngOnInit(): void {
        this.form.patchValue({
            amount: this.data.outstandingAmount
        });

        this.loadFundingAccounts();
    }

    get selectedAccount(): FundingAccount | undefined {
        return this.fundingAccounts.find(
            a => a.id === this.form.controls.fundingAccountId.value
        );
    }

    submit(): void {

        if (this.form.invalid || this.saving) {
            this.form.markAllAsTouched();
            return;
        }

        this.saving = true;

        this.service
            .settle(
                this.data.expenseId,
                {
                    fundingAccountId: this.form.controls.fundingAccountId.value,
                    amount: Number(this.form.controls.amount.value),
                    settlementDate: this.form.controls.settlementDate.value,
                    reference: this.form.controls.reference.value || undefined
                }
            )
            .pipe(
                finalize(() => this.saving = false)
            )
            .subscribe({
                next: () => {
                    this.snack.open('Expense settled successfully', 'Close', { duration: 3000 });
                    this.dialogRef.close(true);
                },
                error: err => {
                    this.snack.open(err?.error.message || 'Settlement failed', 'Close', { duration: 6000 });
                }
            });
    }

    private loadFundingAccounts(): void {

        this.loading = true;

        this.apPaymentService
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
                        err?.message || 'Failed to load funding accounts',
                        'Close',
                        { duration: 6000 }
                    );
                }
            });
    }
}