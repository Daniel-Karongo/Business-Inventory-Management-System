import { CommonModule } from '@angular/common';
import {
    Component,
    EventEmitter,
    Input,
    Output
} from '@angular/core';

import { MatButtonModule } from '@angular/material/button';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import {
    PackagingDTO
} from '../../../../models/packaging.model';

import {
    ProductVariantPackagingService
} from '../../services/product-variant-packaging.service';

import {
    PackagingFormComponent
} from '../packaging-form/packaging-form.component';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
    selector: 'app-variant-packaging-manager',
    standalone: true,
    imports: [
        CommonModule,
        MatButtonModule,
        MatDialogModule,
        MatIconModule,
        MatSnackBarModule,
        MatTooltipModule
    ],
    templateUrl: './variant-packaging-manager.component.html',
    styleUrls: ['./variant-packaging-manager.component.scss']
})
export class VariantPackagingManagerComponent {

    @Input({ required: true }) variantId!: string;
    @Input() branchId?: string;
    @Output()
    refreshRequested =
        new EventEmitter<void>();

    @Input({ required: true })
    packagings: PackagingDTO[] = [];

    constructor(
        private service: ProductVariantPackagingService,
        private dialog: MatDialog,
        private snackbar: MatSnackBar
    ) { }

    create() {

        const ref = this.dialog.open(
            PackagingFormComponent,
            {
                width: 'min(640px, 96vw)',
                maxWidth: '96vw',
                panelClass: 'enterprise-dialog',
                autoFocus: false
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
                        {
                            duration: 3000
                        }
                    );

                    this.refreshRequested.emit();
                }
            });
        });
    }

    edit(packaging: PackagingDTO) {

        const ref = this.dialog.open(
            PackagingFormComponent,
            {
                width: 'min(640px, 96vw)',
                maxWidth: '96vw',
                panelClass: 'enterprise-dialog',
                autoFocus: false,
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

                    this.refreshRequested.emit();
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

                this.snackbar.open(
                    'Packaging deleted',
                    'Close',
                    {
                        duration: 3000
                    }
                );

                this.refreshRequested.emit();
            }
        });
    }
}