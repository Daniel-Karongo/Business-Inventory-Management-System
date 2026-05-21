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
    SupplierBill
} from '../../models/supplier-bill.model';

@Component({
    selector: 'app-invoice-details-dialog',
    standalone: true,
    imports: [
        CommonModule,

        CurrencyPipe,
        DatePipe,

        MatDialogModule,
        MatButtonModule,
        MatDividerModule
    ],
    templateUrl:
        './invoice-details-dialog.component.html',
    styleUrls: [
        './invoice-details-dialog.component.scss'
    ]
})
export class InvoiceDetailsDialogComponent {

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public bill: SupplierBill
    ) { }
}