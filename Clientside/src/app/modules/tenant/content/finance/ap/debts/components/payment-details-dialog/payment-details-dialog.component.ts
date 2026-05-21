import {
    CommonModule,
    CurrencyPipe,
    DatePipe
} from '@angular/common';

import {
    Component,
    Inject
} from '@angular/core';

import {
    MAT_DIALOG_DATA,
    MatDialogModule
} from '@angular/material/dialog';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatDividerModule
} from '@angular/material/divider';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    SupplierPayment
} from '../../models/supplier-payment.model';

@Component({
    selector: 'app-payment-details-dialog',
    standalone: true,
    imports: [
        CommonModule,

        CurrencyPipe,
        DatePipe,

        MatDialogModule,
        MatButtonModule,
        MatDividerModule,
        MatIconModule
    ],
    templateUrl:
        './payment-details-dialog.component.html',
    styleUrls: [
        './payment-details-dialog.component.scss'
    ]
})
export class PaymentDetailsDialogComponent {

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public payment: SupplierPayment
    ) { }

    get allocationRate(): number {

        if (!this.payment.amount) {
            return 0;
        }

        return (
            this.payment.allocatedAmount
            /
            this.payment.amount
        ) * 100;
    }
}