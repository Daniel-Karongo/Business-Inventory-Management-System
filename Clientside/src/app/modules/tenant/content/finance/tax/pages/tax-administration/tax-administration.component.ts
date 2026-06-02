import {
    Component,
    OnDestroy,
    OnInit
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    FormsModule
} from '@angular/forms';

import {
    Subject,
    takeUntil
} from 'rxjs';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatSnackBar,
    MatSnackBarModule
} from '@angular/material/snack-bar';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatInputModule
} from '@angular/material/input';

import {
    WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
    WorkflowCardComponent
} from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
    BranchContextService
} from '../../../../../../../core/services/branch-context.service';

import {
    TaxService
} from '../../services/tax.service';
import { BranchListItemDTO } from '../../../../branches/models/branch.model';
import { BranchService } from '../../../../branches/services/branch.service';

@Component({
    standalone: true,
    selector: 'app-tax-administration',
    templateUrl: './tax-administration.component.html',
    styleUrls: ['./tax-administration.component.scss'],
    imports: [
        CommonModule,
        FormsModule,
        MatButtonModule,
        MatIconModule,
        MatSnackBarModule,
        MatFormFieldModule,
        MatSelectModule,
        MatInputModule,
        WorkflowShellComponent,
        WorkflowCardComponent
    ]
})
export class TaxAdministrationComponent
    implements OnInit, OnDestroy {

    private destroy$ =
        new Subject<void>();

    loading = false;
    saving = false;
    locking = false;

    branches:
        BranchListItemDTO[] = [];

    selectedBranchId = '';

    status: any = null;

    vatEnabled = true;
    vatRate = 16;
    corporateTaxRate = 30;

    constructor(
        private readonly taxService: TaxService,
        private readonly branchService: BranchService,
        private readonly branchContext: BranchContextService,
        private readonly snackbar: MatSnackBar
    ) { }

    ngOnInit(): void {

        this.loadBranches();
    }

    ngOnDestroy(): void {

        this.destroy$.next();
        this.destroy$.complete();
    }

    loadBranches() {

        this.branchService
            .getAllLegacy()
            .pipe(
                takeUntil(this.destroy$)
            )
            .subscribe({
                next: branches => {

                    this.branches = branches;

                    const current =
                        this.branchContext.currentBranchId();

                    this.selectedBranchId =
                        current
                        ?? branches[0]?.id
                        ?? '';

                    if (this.selectedBranchId) {
                        this.loadStatus(
                            this.selectedBranchId
                        );
                    }
                }
            });
    }

    onBranchChanged(
        branchId: string
    ) {

        if (!branchId) {
            return;
        }

        this.branchContext.setBranch(
            branchId
        );

        this.loadStatus(
            branchId
        );
    }

    loadStatus(
        branchId: string
    ) {

        this.loading = true;

        this.taxService
            .getStatus(branchId)
            .subscribe({
                next: status => {

                    this.status = status;

                    this.vatEnabled =
                        status?.vatEnabled ?? true;

                    this.vatRate =
                        status?.vatRate ?? 16;

                    this.corporateTaxRate =
                        status?.corporateTaxRate ?? 30;

                    this.loading = false;
                },
                error: () => {

                    this.loading = false;

                    this.snackbar.open(
                        'Failed to load tax configuration',
                        'Close',
                        { duration: 3000 }
                    );
                }
            });
    }

    save() {

        if (!this.selectedBranchId) {
            return;
        }

        this.saving = true;

        this.taxService
            .configure({
                branchId: this.selectedBranchId,
                vatEnabled: this.vatEnabled,
                vatRate: this.vatRate,
                corporateTaxRate:
                    this.corporateTaxRate
            })
            .subscribe({
                next: () => {

                    this.saving = false;

                    this.snackbar.open(
                        'Tax configuration saved',
                        'Close',
                        { duration: 2500 }
                    );

                    this.loadStatus(
                        this.selectedBranchId
                    );
                },
                error: () => {

                    this.saving = false;

                    this.snackbar.open(
                        'Failed to save configuration',
                        'Close',
                        { duration: 3000 }
                    );
                }
            });
    }

    lockSystem() {

        if (
            !this.selectedBranchId ||
            this.status?.locked
        ) {
            return;
        }

        this.locking = true;

        this.taxService
            .lock(
                this.selectedBranchId
            )
            .subscribe({
                next: () => {

                    this.locking = false;

                    this.snackbar.open(
                        'Tax system locked',
                        'Close',
                        { duration: 2500 }
                    );

                    this.loadStatus(
                        this.selectedBranchId
                    );
                },
                error: () => {

                    this.locking = false;

                    this.snackbar.open(
                        'Failed to lock system',
                        'Close',
                        { duration: 3000 }
                    );
                }
            });
    }
}