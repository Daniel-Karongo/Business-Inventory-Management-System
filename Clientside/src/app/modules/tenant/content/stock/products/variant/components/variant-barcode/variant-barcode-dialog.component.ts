import { CommonModule } from '@angular/common';
import {
    Component,
    Inject,
    OnDestroy,
    OnInit
} from '@angular/core';
import {
    MAT_DIALOG_DATA,
    MatDialogModule
} from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { ProductVariantService } from '../../services/product-variant.service';

@Component({
    selector: 'app-variant-barcode-dialog',
    standalone: true,
    imports: [
        CommonModule,
        MatDialogModule,
        MatButtonModule,
        MatIconModule,
        MatProgressSpinnerModule
    ],
    templateUrl:
        './variant-barcode-dialog.component.html',
    styleUrls: [
        './variant-barcode-dialog.component.scss'
    ]
})
export class VariantBarcodeDialogComponent
    implements OnInit, OnDestroy {

    loading = true;

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
            .subscribe({

                next: blob => {

                    if (blob) {

                        this.barcodeUrl =
                            URL.createObjectURL(
                                blob
                            );
                    }

                    this.loading = false;
                },

                error: () => {

                    this.loading = false;
                }
            });
    }

    ngOnDestroy(): void {

        if (this.barcodeUrl) {

            URL.revokeObjectURL(
                this.barcodeUrl
            );
        }
    }

    download() {

        if (!this.barcodeUrl) {
            return;
        }

        const a =
            document.createElement('a');

        a.href =
            this.barcodeUrl;

        a.download =
            `${this.data.variantName}-barcode.png`;

        a.click();
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
                <head>
                    <title>${this.data.variantName}</title>
                    <style>
                        body{
                            margin:0;
                            display:flex;
                            justify-content:center;
                            align-items:center;
                            height:100vh;
                        }

                        img{
                            max-width:90%;
                            max-height:90%;
                        }
                    </style>
                </head>
                <body>
                    <img src="${this.barcodeUrl}">
                </body>
            </html>
        `);

        win.document.close();

        win.focus();

        setTimeout(() => {

            win.print();

        }, 300);
    }
}