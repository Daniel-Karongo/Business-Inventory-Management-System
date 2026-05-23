import { Injectable } from '@angular/core';

import { ReceiptPrintData } from '../models/receipt-print.model';

import { PrintWindowService } from './print-window.service';
import { InvoiceA4PrintComponent } from '../components/invoice-a4-print/invoice-a4-print.component';
import { Thermal80PrintComponent } from '../components/thermal-80-print/thermal-80-print.component';
import { Thermal58PrintComponent } from '../components/thermal-58-print/thermal-58-print.component';

@Injectable({
    providedIn: 'root'
})
export class EnterprisePrintService {

    constructor(
        private readonly printWindowService: PrintWindowService
    ) { }

    async printA4(
        data: ReceiptPrintData
    ): Promise<void> {

        await this.printWindowService.printComponent(
            InvoiceA4PrintComponent,
            { data },
            `Invoice-${data.receiptNumber}`
        );
    }

    async printThermal80(
        data: ReceiptPrintData
    ): Promise<void> {

        await this.printWindowService.printComponent(
            Thermal80PrintComponent,
            { data },
            `Receipt-${data.receiptNumber}`
        );
    }

    async printThermal58(
        data: ReceiptPrintData
    ): Promise<void> {

        await this.printWindowService.printComponent(
            Thermal58PrintComponent,
            { data },
            `Receipt-${data.receiptNumber}`
        );
    }
}