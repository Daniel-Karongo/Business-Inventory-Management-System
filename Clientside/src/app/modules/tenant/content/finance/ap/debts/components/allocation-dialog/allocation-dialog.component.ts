import {
    CommonModule,
    CurrencyPipe,
    DatePipe
} from '@angular/common';

import {
    Component,
    Inject,
    inject
} from '@angular/core';

import {
    FormControl,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import {
    MAT_DIALOG_DATA,
    MatDialogModule,
    MatDialogRef
} from '@angular/material/dialog';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatSnackBar,
    MatSnackBarModule
} from '@angular/material/snack-bar';

import {
    finalize,
    Observable
} from 'rxjs';

import {
    ApAllocationService
} from '../../services/ap-allocation.service';

import {
    SupplierBill
} from '../../models/supplier-bill.model';

import {
    SupplierPaymentResponseDto
} from '../../models/supplier-payment.model';

import {
    AllocationPreviewResponse
} from '../../models/allocation.model';

import {
    MatDialog
} from '@angular/material/dialog';
import { ConfirmDialogComponent } from '../../../../../../../../shared/components/confirm-dialog/confirm-dialog.component';

export interface AllocationDialogData {
    branchId: string;
    supplierId: string;
    payments: SupplierPaymentResponseDto[];
    bills: SupplierBill[];
}

@Component({
    selector: 'app-allocation-dialog',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,

        CurrencyPipe,
        DatePipe,

        MatDialogModule,
        MatButtonModule,
        MatIconModule,
        MatInputModule,
        MatFormFieldModule,
        MatSelectModule,
        MatSnackBarModule
    ],
    templateUrl: './allocation-dialog.component.html',
    styleUrls: ['./allocation-dialog.component.scss']
})
export class AllocationDialogComponent {

    private allocationService =
        inject(ApAllocationService);

    private snackBar =
        inject(MatSnackBar);

    private dialog =
        inject(MatDialog);

    loading = false;

    previewLoading = false;

    preview?: AllocationPreviewResponse;

    selectedPayment?: SupplierPaymentResponseDto;

    selectedInvoice?: SupplierBill;

    submitting = false;

    readonly paymentControl =
        new FormControl<string | null>(
            null,
            Validators.required
        );

    readonly invoiceControl =
        new FormControl<string | null>(
            null
        );

    readonly amountControl =
        new FormControl<number | null>(
            null,
            [
                Validators.required,
                Validators.min(0.01)
            ]
        );

    readonly modeControl =
        new FormControl<'manual' | 'auto'>(
            'auto',
            {
                nonNullable: true
            }
        );

    constructor(
        private ref: MatDialogRef<
            AllocationDialogComponent
        >,

        @Inject(MAT_DIALOG_DATA)
        public data: AllocationDialogData
    ) { }

    get availablePayments(): SupplierPaymentResponseDto[] {

        return this.data.payments.filter(payment =>

            payment.posted
            &&

            !payment.reversed
            &&

            payment.unappliedAmount > 0
        );
    }

    selectPayment(
        paymentId: string | null
    ): void {

        this.selectedPayment =
            this.data.payments.find(
                p => p.id === paymentId
            );

        this.preview = undefined;

        if (!this.selectedPayment) {

            this.amountControl.setValue(null);

            return;
        }

        const unapplied =
            Number(
                this.selectedPayment.unappliedAmount
            );

        this.amountControl.setValue(
            unapplied > 0
                ? unapplied
                : null
        );

        this.amountControl.setValidators([
            Validators.required,
            Validators.min(0.01),
            Validators.max(unapplied)
        ]);

        this.amountControl.updateValueAndValidity();
    }

    selectInvoice(
        invoiceId: string | null
    ): void {

        this.selectedInvoice =
            this.data.bills.find(
                b => b.invoiceId === invoiceId
            );
    }

    previewAllocation(): void {

        if (
            !this.selectedPayment ||
            !this.amountControl.value
        ) {
            return;
        }

        this.previewLoading = true;

        this.allocationService
            .previewAutoAllocation({
                branchId:
                    this.data.branchId,

                supplierId:
                    this.data.supplierId,

                paymentId:
                    this.selectedPayment.id,

                amount:
                    this.amountControl.value
            })
            .pipe(
                finalize(() =>
                    this.previewLoading = false
                )
            )
            .subscribe({
                next: res =>
                    this.preview = res,

                error: err =>
                    this.snackBar.open(
                        err?.error?.message ??
                        'Failed to preview allocation',
                        'Close',
                        { duration: 4000 }
                    )
            });
    }

    submit(): void {

        if (
            this.loading ||
            this.submitting
        ) {
            return;
        }

        if (
            !this.selectedPayment ||
            !this.amountControl.valid
        ) {

            this.snackBar.open(
                'Provide a valid allocation amount',
                'Close',
                { duration: 3500 }
            );

            return;
        }

        if (
            this.selectedPayment.unappliedAmount <= 0
        ) {

            this.snackBar.open(
                'Payment has no unapplied balance',
                'Close',
                { duration: 4000 }
            );

            return;
        }

        if (
            this.amountControl.value! >
            this.selectedPayment.unappliedAmount
        ) {

            this.snackBar.open(
                'Allocation exceeds unapplied balance',
                'Close',
                { duration: 4000 }
            );

            return;
        }

        if (
            this.modeControl.value === 'manual'
            &&
            !this.selectedInvoice
        ) {

            this.snackBar.open(
                'Select an invoice',
                'Close',
                { duration: 3500 }
            );

            return;
        }

        this.dialog.open(
            ConfirmDialogComponent,
            {
                width: '420px',
                data: {
                    title: 'Confirm Allocation',
                    message:
                        'This allocation will permanently affect supplier balances.',
                    confirmText: 'Allocate'
                }
            }
        )
            .afterClosed()
            .subscribe(confirmed => {

                if (!confirmed) {
                    return;
                }

                this.execute();
            });
    }

    private execute(): void {

        if (
            !this.selectedPayment ||
            !this.amountControl.value
        ) {
            return;
        }

        this.loading = true;
        this.submitting = true;

        const complete = () => {

            this.snackBar.open(
                'Allocation completed',
                'Close',
                { duration: 3500 }
            );

            this.ref.close(true);
        };

        const fail = (err: any) => {

            const message =
                err?.error?.message ??
                'Allocation failed';

            this.snackBar.open(
                message,
                'Close',
                { duration: 5000 }
            );
        };

        const finish = () => {

            this.loading = false;
            this.submitting = false;
        };

        if (this.modeControl.value === 'auto') {

            this.allocationService
                .autoAllocate({
                    branchId:
                        this.data.branchId,

                    supplierId:
                        this.data.supplierId,

                    paymentId:
                        this.selectedPayment.id,

                    amount:
                        this.amountControl.value
                })
                .pipe(
                    finalize(finish)
                )
                .subscribe({
                    next: complete,
                    error: fail
                });

            return;
        }

        this.allocationService
            .manualAllocate({
                branchId:
                    this.data.branchId,

                supplierId:
                    this.data.supplierId,

                paymentId:
                    this.selectedPayment.id,

                purchaseInvoiceId:
                    this.selectedInvoice!.invoiceId,

                allocationAmount:
                    this.amountControl.value
            })
            .pipe(
                finalize(finish)
            )
            .subscribe({
                next: complete,
                error: fail
            });
    }
}