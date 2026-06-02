import {
    Component,
    OnDestroy,
    OnInit,
    inject
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    Router,
    RouterModule
} from '@angular/router';

import {
    Subject,
    combineLatest,
    takeUntil
} from 'rxjs';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    TaxService
} from '../../services/tax.service';

import {
    TaxStatus
} from '../../models/tax.models';
import { WorkflowShellComponent } from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { BranchService } from '../../../../branches/services/branch.service';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { BranchListItemDTO } from '../../../../branches/models/branch.model';
import { FormsModule } from '@angular/forms';

@Component({
    standalone: true,
    selector: 'app-tax-dashboard',
    imports: [
        CommonModule,
        RouterModule,
        FormsModule,
        MatButtonModule,
        MatIconModule,
        MatFormFieldModule,
        MatSelectModule,
        WorkflowShellComponent
    ],
    templateUrl: './tax-dashboard.component.html',
    styleUrls: ['./tax-dashboard.component.scss']
})
export class TaxDashboardComponent
    implements OnInit, OnDestroy {

    private readonly destroy$ =
        new Subject<void>();

    private readonly branchService =
        inject(BranchService);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly taxService =
        inject(TaxService);

    private readonly router =
        inject(Router);

    loading = true;

    selectedBranchId:
        string | null = null;

    branches:
        BranchListItemDTO[] = [];

    status:
        TaxStatus | null = null;

    activeTab:
        'overview'
        | 'vat'
        | 'corporate'
        | 'administration'
        = 'overview';

    readonly tabs = [
        {
            key: 'overview',
            label: 'Overview',
            icon: 'dashboard'
        },
        {
            key: 'vat',
            label: 'VAT',
            icon: 'receipt_long'
        },
        {
            key: 'corporate',
            label: 'Corporate Tax',
            icon: 'account_balance'
        },
        {
            key: 'administration',
            label: 'Administration',
            icon: 'admin_panel_settings'
        }
    ];

    ngOnInit(): void {

        this.loadBranches();

        this.branchContext.branch$
            .pipe(
                takeUntil(
                    this.destroy$
                )
            )
            .subscribe(branchId => {

                this.selectedBranchId =
                    branchId;

                if (branchId) {
                    this.loadStatus(
                        branchId
                    );
                }

            });

    }

    ngOnDestroy(): void {

        this.destroy$.next();

        this.destroy$.complete();

    }

    private loadBranches() {

        this.branchService
            .getAllLegacy()
            .pipe(
                takeUntil(
                    this.destroy$
                )
            )
            .subscribe({

                next: branches => {

                    this.branches =
                        branches;

                    this.branchContext
                        .setBranches(
                            branches
                        );

                    if (
                        branches.length === 1
                    ) {

                        this.selectedBranchId =
                            branches[0].id;

                        this.branchContext
                            .setBranch(
                                branches[0].id
                            );

                    }

                    const current =
                        this.branchContext
                            .currentBranch;

                    if (
                        current &&
                        !this.selectedBranchId
                    ) {

                        this.selectedBranchId =
                            current;

                    }

                }

            });

    }

    private loadStatus(
        branchId: string
    ) {

        this.loading = true;

        this.taxService
            .getStatus(branchId)
            .subscribe({

                next: status => {

                    this.status =
                        status;

                    this.loading =
                        false;

                },

                error: () => {

                    this.loading =
                        false;

                }

            });

    }

    onBranchChanged(
        branchId: string
    ) {
        this.branchContext.setBranch(
            branchId
        );
    }

    selectTab(
        key: any
    ) {

        this.activeTab = key;

    }

    openVat() {

        this.router.navigate([
            '/app/finance/tax/vat'
        ]);

    }

    openCorporate() {

        this.router.navigate([
            '/app/finance/tax/corporate'
        ]);

    }

    openAdministration() {

        this.router.navigate([
            '/app/finance/tax/administration'
        ]);

    }

    get currentBranch() {

        return this.branches.find(
            x => x.id ===
                this.selectedBranchId
        );

    }

    get branchSelectorVisible() {

        return this.branches.length > 1;

    }

}