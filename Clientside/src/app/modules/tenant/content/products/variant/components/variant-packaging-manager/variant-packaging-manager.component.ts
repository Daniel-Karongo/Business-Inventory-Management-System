import { CommonModule } from '@angular/common';
import {
    Component,
    Input,
    OnInit
} from '@angular/core';

import { MatButtonModule } from '@angular/material/button';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import {
    PackagingDTO
} from '../../../../stock/models/packaging.model';

import {
    ProductVariantPackagingService
} from '../../services/product-variant-packaging.service';

import {
    PackagingFormComponent
} from '../packaging-form/packaging-form.component';

@Component({
    selector: 'app-variant-packaging-manager',
    standalone: true,
    imports: [
        CommonModule,
        MatButtonModule,
        MatDialogModule,
        MatIconModule,
        MatSnackBarModule
    ],
    templateUrl: './variant-packaging-manager.component.html',
    styleUrls: ['./variant-packaging-manager.component.scss']
})
export class VariantPackagingManagerComponent implements OnInit {

    @Input({ required: true }) variantId!: string;
    @Input() branchId?: string;

    loading = true;

    packagings: PackagingDTO[] = [];

    constructor(
        private service: ProductVariantPackagingService,
        private dialog: MatDialog,
        private snackbar: MatSnackBar
    ) { }

    ngOnInit(): void {
        this.load();
    }

    load() {

        this.loading = true;

        this.service.getForVariant(this.variantId).subscribe({
            next: res => {
                this.packagings = res ?? [];
                this.loading = false;
            },
            error: () => {
                this.loading = false;
            }
        });
    }

    create() {

        const ref = this.dialog.open(
            PackagingFormComponent,
            {
                width: '500px'
            }
        );

        ref.afterClosed().subscribe(payload => {

            if (!payload) return;

            this.service.create({
                variantId: this.variantId,
                ...payload
            }).subscribe({
                next: () => {
                    this.snackbar.open(
                        'Packaging created',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                }
            });
        });
    }

    edit(packaging: PackagingDTO) {

        const ref = this.dialog.open(
            PackagingFormComponent,
            {
                width: '500px',
                data: {
                    packaging,
                    editMode: true
                }
            }
        );

        ref.afterClosed().subscribe(payload => {

            if (!payload) return;

            this.service.update(
                packaging.packagingId,
                payload
            ).subscribe({
                next: () => {
                    this.snackbar.open(
                        'Packaging updated',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                }
            });
        });
    }

    remove(packaging: PackagingDTO) {

        if (
            packaging.isBaseUnit ||
            !this.branchId
        ) {
            return;
        }

        this.service.remove(
            packaging.packagingId,
            this.branchId
        ).subscribe({
            next: () => {

                this.packagings =
                    this.packagings.filter(
                        p => p.packagingId !== packaging.packagingId
                    );

                this.snackbar.open(
                    'Packaging deleted',
                    'Close',
                    { duration: 3000 }
                );
            }
        });
    }
}