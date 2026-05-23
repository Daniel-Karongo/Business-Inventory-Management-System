import {
    ChangeDetectionStrategy,
    Component,
    Input,
    ViewEncapsulation
} from '@angular/core';

import { CommonModule } from '@angular/common';

import { ReceiptPrintData }
    from '../../models/receipt-print.model';

@Component({
    standalone: true,
    selector: 'app-invoice-a4-print',
    imports: [CommonModule],
    templateUrl: './invoice-a4-print.component.html',
    styleUrls: ['./invoice-a4-print.component.scss'],
    encapsulation: ViewEncapsulation.Emulated,
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class InvoiceA4PrintComponent {

    @Input({ required: true })
    data!: ReceiptPrintData;
}