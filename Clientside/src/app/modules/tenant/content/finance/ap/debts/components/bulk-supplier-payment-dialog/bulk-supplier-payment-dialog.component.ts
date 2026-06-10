import {
    CommonModule
} from '@angular/common';
import {
    Component,
    Inject,
    inject
} from '@angular/core';
import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';
import {
    MatButtonModule
} from '@angular/material/button';
import {
    MatCheckboxModule
} from '@angular/material/checkbox';
import {
    MatDatepickerModule
} from '@angular/material/datepicker';
import {
    MAT_DIALOG_DATA,
    MatDialogModule,
    MatDialogRef
} from '@angular/material/dialog';
import {
    MatFormFieldModule
} from '@angular/material/form-field';
import {
    MatIconModule
} from '@angular/material/icon';
import {
    MatInputModule
} from '@angular/material/input';
import {
    MatSelectModule
} from '@angular/material/select';
import {
    MatSnackBar,
    MatSnackBarModule
} from '@angular/material/snack-bar';
import {
    finalize
} from 'rxjs';

import { FundingAccount } from '../../models/funding-account.model';
import { SupplierDebtSummary } from '../../models/supplier-debt-summary.model';
import { ApPaymentService } from '../../services/ap-payment.service';

@Component({
    selector: 'app-bulk-supplier-payment-dialog',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatDialogModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatCheckboxModule,
        MatDatepickerModule,
        MatIconModule,
        MatSnackBarModule
    ],
    templateUrl:
        './bulk-supplier-payment-dialog.component.html',
    styleUrls: [
        './bulk-supplier-payment-dialog.component.scss'
    ]
})
export class BulkSupplierPaymentDialogComponent {

    private fb =
        inject(FormBuilder);

    private payments =
        inject(ApPaymentService);

    private snack =
        inject(MatSnackBar);

    loading = false;

    accounts: FundingAccount[] = [];

    readonly form =
        this.fb.nonNullable.group({
            fundingAccountId: [
                '',
                Validators.required
            ],
            reference: [''],
            paymentDate: [
                new Date(),
                Validators.required
            ],
            autoAllocate: [true]
        });

    constructor(
        private ref:
            MatDialogRef<
                BulkSupplierPaymentDialogComponent
            >,
        @Inject(MAT_DIALOG_DATA)
        public data: {
            branchId: string;
            suppliers:
            SupplierDebtSummary[];
        }
    ) { }

    ngOnInit(): void {

        this.loading = true;

        this.payments
            .fundingAccounts()
            .pipe(
                finalize(
                    () =>
                        this.loading = false
                )
            )
            .subscribe({
                next: accounts => {

                    this.accounts =
                        [...accounts]
                            .sort(
                                (a, b) =>
                                    a.code.localeCompare(
                                        b.code
                                    )
                            );

                    if (!this.accounts.length) {
                        return;
                    }

                    const preferred =
                        this.accounts.find(a =>
                            (a.role || '')
                                .toUpperCase()
                                .includes('CASH')
                        )
                        ?? this.accounts.find(a =>
                            (a.role || '')
                                .toUpperCase()
                                .includes('BANK')
                        )
                        ?? this.accounts[0];

                    this.form.patchValue({
                        fundingAccountId: preferred.id
                    });
                },

                error:
                    err =>
                        this.error(err)
            });
    }

    get requiresAutoFunding(): boolean {

        return !!this.selectedAccount
            && this.selectedBalance < this.totalAmount;

    }

    get fundingShortfall(): number {

        return Math.max(
            0,
            this.totalAmount - this.selectedBalance
        );

    }

    get selectedBalance(): number {

        return (
            this.selectedAccount?.balance
            || 0
        );
    }

    get supplierCount(): number {

        return this.data.suppliers.length;
    }

    get totalAmount(): number {

        return this.data.suppliers
            .reduce(
                (
                    total,
                    supplier
                ) =>
                    total +
                    Number(
                        supplier.netPayable || 0
                    ),
                0
            );
    }

    get selectedAccount():
        FundingAccount | undefined {

        const id =
            this.form
                .get(
                    'fundingAccountId'
                )
                ?.value;

        return this.accounts.find(
            account =>
                account.id === id
        );
    }

    get derivedMethod():
        string {

        return (
            this.selectedAccount?.role
            ?? '-'
        );
    }

    displayAccount(
        account: FundingAccount
    ): string {

        return `${account.code} - ${account.name} (KES ${account.balance.toLocaleString(
            undefined,
            {
                minimumFractionDigits: 2,
                maximumFractionDigits: 2
            }
        )})`;
    }

    submit(): void {

        if (
            this.loading
        ) {
            return;
        }

        if (
            this.loading
            ||
            this.form.invalid
        ) {
            return;
        }

        if (
            !this.selectedAccount
        ) {
            return;
        }

        this.loading = true;

        const value =
            this.form
                .getRawValue();

        this.payments
            .bulkProcess({
                branchId:
                    this.data.branchId,
                supplierIds:
                    this.data.suppliers
                        .map(
                            supplier =>
                                supplier.supplierId
                        ),
                fundingAccountId:
                    value.fundingAccountId,
                method:
                    this.selectedAccount
                        .role as any,
                reference:
                    value.reference
                    || undefined,
                paymentDate:
                    new Date(
                        value.paymentDate
                    )
                        .toISOString()
                        .split('T')[0],
                autoAllocate:
                    value.autoAllocate
            })
            .pipe(
                finalize(
                    () =>
                        this.loading = false
                )
            )
            .subscribe({
                next:
                    results => {

                        const success =
                            results.filter(
                                r =>
                                    r.status ===
                                    'SUCCESS'
                            ).length;

                        const failedResults =
                            results.filter(
                                r => r.status === 'FAILED'
                            );

                        if (
                            failedResults.length === 0
                        ) {

                            this.success(
                                `${success} payment(s) processed successfully`
                            );

                        }
                        else if (
                            success === 0
                        ) {

                            this.failure(
                                failedResults
                                    .map(
                                        r =>
                                            `${r.supplierName ?? r.supplierId}: ${r.message}`
                                    )
                                    .join(' | ')
                            );

                        }
                        else {

                            this.snack.open(
                                `${success} succeeded, ${failedResults.length} failed`,
                                'Close',
                                {
                                    duration: 8000,
                                    panelClass: [
                                        'snackbar-warning'
                                    ]
                                }
                            );
                        }

                        this.ref.close(
                            true
                        );
                    },
                error: err => {

                    this.error(err);

                    this.ref.close();

                }
            });
    }

    private error(
        err: any
    ): void {

        this.failure(
            err?.message
            ||
            'Operation failed'
        );
    }

    private success(
        message: string
    ): void {

        this.snack.open(
            message,
            'Close',
            {
                duration: 5000,
                panelClass: [
                    'snackbar-success'
                ]
            }
        );
    }

    private failure(
        message: string
    ): void {

        this.snack.open(
            message,
            'Close',
            {
                duration: 8000,
                panelClass: [
                    'snackbar-error'
                ]
            }
        );
    }
}