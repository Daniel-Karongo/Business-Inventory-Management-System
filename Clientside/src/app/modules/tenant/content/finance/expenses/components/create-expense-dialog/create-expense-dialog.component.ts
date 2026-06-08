import { CommonModule } from '@angular/common';
import { Component, OnInit, inject } from '@angular/core';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar } from '@angular/material/snack-bar';
import { finalize } from 'rxjs';

import { OperationalExpenseService } from '../../services/operational-expense.service';
import { AccountsService } from '../../../accounts/services/accounts.service';
import { ApPaymentService } from '../../../ap/debts/services/ap-payment.service';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { Account } from '../../../accounts/models/account.models';
import { FundingAccount } from '../../../ap/debts/models/funding-account.model';

@Component({
    selector: 'app-create-expense-dialog',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatDialogModule,
        MatButtonModule,
        MatCheckboxModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule
    ],
    templateUrl: './create-expense-dialog.component.html',
    styleUrls: ['./create-expense-dialog.component.scss']
})
export class CreateExpenseDialogComponent implements OnInit {

    private readonly fb = inject(FormBuilder);
    private readonly service = inject(OperationalExpenseService);
    private readonly accountsService = inject(AccountsService);
    private readonly paymentService = inject(ApPaymentService);
    private readonly branchContext = inject(BranchContextService);
    private readonly snack = inject(MatSnackBar);

    loading = false;
    saving = false;

    expenseAccounts: Account[] = [];
    fundingAccounts: FundingAccount[] = [];

    readonly form = this.fb.nonNullable.group({
        expenseAccountId: ['', Validators.required],
        fundingAccountId: [''],
        description: ['', [Validators.required, Validators.maxLength(500)]],
        amount: [0, [Validators.required, Validators.min(0.01)]],
        accountingDate: [
            new Date().toISOString().substring(0, 10),
            Validators.required
        ],
        autoPay: [false],
        reference: ['']
    });

    ngOnInit(): void {

        this.loadExpenseAccounts();
        this.loadFundingAccounts();

        this.form.controls.autoPay
            .valueChanges
            .subscribe(autoPay => {

                const funding =
                    this.form.controls.fundingAccountId;

                if (autoPay) {
                    funding.addValidators(
                        Validators.required
                    );
                } else {
                    funding.clearValidators();
                }

                funding.updateValueAndValidity();
            });
    }

    get selectedFundingAccount(): FundingAccount | undefined {

        return this.fundingAccounts.find(
            x =>
                x.id ===
                this.form.controls.fundingAccountId.value
        );
    }

    submit(): void {

        if (this.form.invalid || this.saving) {
            this.form.markAllAsTouched();
            return;
        }

        const branchId =
            this.branchContext.currentBranch;

        if (!branchId) {
            return;
        }

        this.saving = true;

        this.service
            .create({
                branchId,
                expenseAccountId:
                    this.form.controls.expenseAccountId.value,
                fundingAccountId:
                    this.form.controls.autoPay.value
                        ? this.form.controls.fundingAccountId.value
                        : undefined,
                description:
                    this.form.controls.description.value,
                amount:
                    Number(this.form.controls.amount.value),
                accountingDate:
                    this.form.controls.accountingDate.value,
                autoPay:
                    this.form.controls.autoPay.value,
                reference:
                    this.form.controls.reference.value || undefined,
                sourceModule:
                    'MANUAL_EXPENSE',
                sourceId:
                    crypto.randomUUID()
            })
            .pipe(
                finalize(() =>
                    this.saving = false
                )
            )
            .subscribe({
                next: () => {

                    this.snack.open(
                        'Expense created successfully',
                        'Close',
                        { duration: 3500 }
                    );

                    this.dialogRef.close(true);
                },
                error: err => {

                    this.snack.open(
                        err?.message || 'Failed to create expense',
                        'Close',
                        { duration: 6000 }
                    );
                }
            });
    }

    private loadExpenseAccounts(): void {

        this.accountsService
            .list({
                page: 0,
                size: 500
            })
            .subscribe({
                next: res => {

                    this.expenseAccounts =
                        (res.content || [])
                            .filter(a =>
                                a.type === 'EXPENSE'
                            )
                            .filter(a =>
                                a.role !== 'CORPORATE_TAX_EXPENSE'
                            );
                }
            });
    }

    private loadFundingAccounts(): void {

        this.loading = true;

        this.paymentService
            .fundingAccounts()
            .pipe(
                finalize(() =>
                    this.loading = false
                )
            )
            .subscribe({
                next: accounts =>
                    this.fundingAccounts = accounts
            });
    }

    constructor(
        private readonly dialogRef:
            MatDialogRef<CreateExpenseDialogComponent>
    ) { }
}