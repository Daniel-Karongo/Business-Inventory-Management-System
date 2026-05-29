import {
    Component,
    Inject,
    OnInit
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    MAT_DIALOG_DATA,
    MatDialogModule
} from '@angular/material/dialog';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    ProductVariantService
} from '../../services/product-variant.service';

@Component({
    selector:
        'app-variant-barcode-dialog',
    standalone: true,
    imports: [
        CommonModule,
        MatDialogModule,
        MatButtonModule
    ],
    templateUrl:
        './variant-barcode-dialog.component.html',
    styleUrls: [
        './variant-barcode-dialog.component.scss'
    ]
})
export class VariantBarcodeDialogComponent
    implements OnInit {

    barcodeUrl?: string;

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public data: {
            variantId: string;
            variantName: string;
        },
        private variantService:
            ProductVariantService
    ) { }

    ngOnInit(): void {

        this.variantService
            .getBarcodeImage(
                this.data.variantId
            )
            .subscribe(blob => {

                this.barcodeUrl =
                    URL.createObjectURL(
                        blob
                    );
            });
    }

    print() {

        if (!this.barcodeUrl) {
            return;
        }

        const win =
            window.open(
                '',
                '_blank'
            );

        if (!win) {
            return;
        }

        win.document.write(`
            <html>
            <body style="text-align:center">
                <img src="${this.barcodeUrl}">
            </body>
            </html>
        `);

        win.print();
    }
}