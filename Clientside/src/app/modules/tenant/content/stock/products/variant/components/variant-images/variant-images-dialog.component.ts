import { CommonModule } from '@angular/common';
import {
    Component,
    Inject,
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
    selector: 'app-variant-images-dialog',
    standalone: true,
    imports: [
        CommonModule,
        MatDialogModule,
        MatButtonModule,
        MatIconModule,
        MatProgressSpinnerModule
    ],
    templateUrl:
        './variant-images-dialog.component.html',
    styleUrls: [
        './variant-images-dialog.component.scss'
    ]
})
export class VariantImagesDialogComponent
    implements OnInit {

    loading = true;

    images: {
        fileName: string;
        url: string;
    }[] = [];

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
            .getImages(
                this.data.variantId
            )
            .subscribe({

                next: urls => {

                    this.images =
                        (urls ?? [])
                            .map(url => ({
                                fileName:
                                    url.split('/').pop() ?? '',
                                url
                            }));

                    this.loading = false;
                },

                error: () => {
                    this.loading = false;
                }
            });
    }

    downloadZip() {

        this.variantService
            .downloadImagesZip(
                this.data.variantId
            )
            .subscribe(blob => {

                const url =
                    URL.createObjectURL(blob);

                const a =
                    document.createElement('a');

                a.href = url;

                a.download =
                    `${this.data.variantName}-images.zip`;

                a.click();

                URL.revokeObjectURL(url);
            });
    }
}