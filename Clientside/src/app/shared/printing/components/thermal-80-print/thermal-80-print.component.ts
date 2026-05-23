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
    selector: 'app-thermal-80-print',
    imports: [CommonModule],
    templateUrl: './thermal-80-print.component.html',
    styleUrls: ['./thermal-80-print.component.scss'],
    encapsulation: ViewEncapsulation.Emulated,
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class Thermal80PrintComponent {

    @Input({ required: true })
    data!: ReceiptPrintData;
}