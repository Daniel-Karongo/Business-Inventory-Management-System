import {
    CommonModule,
    CurrencyPipe,
    DatePipe
} from '@angular/common';

import {
    Component,
    OnInit,
    inject
} from '@angular/core';

import {
    FormControl,
    ReactiveFormsModule
} from '@angular/forms';

import {
    ActivatedRoute,
    Router,
    RouterModule
} from '@angular/router';

import {
    debounceTime,
    distinctUntilChanged,
    finalize
} from 'rxjs';

import {
    MatCardModule
} from '@angular/material/card';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    SupplierDebtSummary
} from '../../models/supplier-debt-summary.model';

import {
    ApDebtService
} from '../../services/ap-debt.service';
import { WorkflowShellComponent } from '../../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { WorkflowCardComponent } from '../../../../../../../../shared/layout/workflow-card/workflow-card.component';
import { PageQuery } from '../../../../../../../../core/models/page-query.model';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDialog } from '@angular/material/dialog';
import { BulkSupplierPaymentDialogComponent } from '../../components/bulk-supplier-payment-dialog/bulk-supplier-payment-dialog.component';
import { BranchContextService } from '../../../../../../../../core/services/branch-context.service';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
    selector: 'app-supplier-debt-list',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        RouterModule,
        MatCheckboxModule,
        CurrencyPipe,
        DatePipe,
        MatTooltipModule,
        WorkflowShellComponent,
        WorkflowCardComponent,

        MatCardModule,
        MatIconModule,
        MatButtonModule,
        MatPaginatorModule,
        MatTableModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule
    ],
    templateUrl: './supplier-debt-list.component.html',
    styleUrls: ['./supplier-debt-list.component.scss']
})
export class SupplierDebtListComponent
    implements OnInit {

    private debtService =
        inject(ApDebtService);

    private router =
        inject(Router);

    private route =
        inject(ActivatedRoute);

    private dialog =
        inject(MatDialog);

    private branchContext = inject(BranchContextService);

    private snack =
        inject(MatSnackBar);

    viewMode: 'table' | 'grid' =
        localStorage.getItem(
            'ap-supplier-view'
        ) as any || 'table';

    density:
        'comfortable'
        | 'compact' =
        localStorage.getItem(
            'ap-supplier-density'
        ) as any || 'comfortable';

    loading = false;

    rows: SupplierDebtSummary[] = [];

    total = 0;

    overdueSuppliers = 0;

    highRiskSuppliers = 0;

    displayedColumns = [
        'select',
        'supplier',
        'outstanding',
        'overdue',
        'unapplied',
        'risk',
        'actions'
    ];

    query: PageQuery = {
        page: 0,
        size: 20,
        sortBy: 'overdueAmount',
        direction: 'desc'
    };

    selectedSupplierIds =
        new Set<string>();

    readonly searchControl =
        new FormControl('', {
            nonNullable: true
        });

    readonly riskFilter =
        new FormControl<string | null>(null);

    readonly overdueFilter =
        new FormControl<string>('all', {
            nonNullable: true
        });

    ngOnInit(): void {

        this.restoreQuery();

        this.searchControl.valueChanges
            .pipe(
                debounceTime(300),
                distinctUntilChanged()
            )
            .subscribe(value => {

                this.query.page = 0;
                this.query.search = value;

                this.syncQuery();
            });

        this.riskFilter.valueChanges
            .subscribe(value => {

                this.query.page = 0;

                this.syncQuery();
            });

        this.overdueFilter.valueChanges
            .subscribe(value => {

                this.query.page = 0;

                this.query.hasOverdue =
                    value === 'all'
                        ? undefined
                        : value === 'overdue';

                this.syncQuery();
            });

        this.load();
    }

    get hasSelection(): boolean {
        return this.selectedSupplierIds.size > 0;
    }

    get selectedRows(): SupplierDebtSummary[] {
        return this.rows.filter(
            r =>
                this.selectedSupplierIds.has(
                    r.supplierId
                )
        );
    }

    get isSingleSelection(): boolean {

        return this.selectedSupplierIds.size === 1;

    }

    billLabel(
        count: number
    ): string {

        return count === 1
            ? 'open bill'
            : 'open bills';
    }

    isSelected(
        supplierId: string
    ): boolean {

        return this.selectedSupplierIds.has(
            supplierId
        );
    }

    setViewMode(
        mode: 'table' | 'grid'
    ): void {

        this.viewMode = mode;

        localStorage.setItem(
            'ap-supplier-view',
            mode
        );
    }

    toggleDensity(): void {

        this.density =
            this.density === 'comfortable'
                ? 'compact'
                : 'comfortable';

        localStorage.setItem(
            'ap-supplier-density',
            this.density
        );
    }

    toggleSelection(
        row: SupplierDebtSummary
    ): void {

        if (
            this.selectedSupplierIds.has(
                row.supplierId
            )
        ) {

            this.selectedSupplierIds.delete(
                row.supplierId
            );

            return;
        }

        this.selectedSupplierIds.add(
            row.supplierId
        );
    }

    paySupplier(
        row: SupplierDebtSummary
    ): void {

        this.selectedSupplierIds.clear();

        this.selectedSupplierIds.add(
            row.supplierId
        );

        this.openBulkPayment();
    }

    openBulkPayment(): void {

        const branchId =
            this.branchContext
                .currentBranch;

        if (!branchId) {
            return;
        }

        this.dialog.open(
            BulkSupplierPaymentDialogComponent,
            {
                panelClass: "enterprise-dialog",
                width: '720px',
                data: {
                    branchId,
                    suppliers:
                        this.selectedRows
                }
            }
        )
            .afterClosed()
            .subscribe(
                refreshed => {

                    if (refreshed) {

                        this.selectedSupplierIds.clear();

                        this.success(
                            'Supplier payments processed successfully'
                        );

                        this.load();
                    }
                }
            );
    }

    private restoreQuery(): void {

        const qp =
            this.route.snapshot.queryParamMap;

        this.query.page =
            Number(qp.get('page') ?? 0);

        this.query.size =
            Number(qp.get('size') ?? 20);

        this.query.sortBy =
            qp.get('sortBy') ?? 'overdueAmount';

        this.query.direction =
            (qp.get('direction') as any) ?? 'desc';

        this.query.search =
            qp.get('search') ?? '';

        const overdue =
            qp.get('hasOverdue');

        if (overdue !== null) {
            this.query.hasOverdue =
                overdue === 'true';
        }

        this.searchControl.setValue(
            this.query.search ?? '',
            { emitEvent: false }
        );

        this.overdueFilter.setValue(
            this.query.hasOverdue === undefined
                ? 'all'
                : this.query.hasOverdue
                    ? 'overdue'
                    : 'current',
            { emitEvent: false }
        );
    }

    private syncQuery(): void {

        this.router.navigate(
            [],
            {
                relativeTo: this.route,
                queryParams: {
                    ...this.query
                },
                queryParamsHandling: 'merge'
            }
        );

        this.load();
    }

    load(): void {

        this.loading = true;

        this.debtService
            .getDebtSummary(this.query)
            .pipe(
                finalize(() =>
                    this.loading = false
                )
            )
            .subscribe({
                next: res => {

                    this.rows = res.content;

                    this.total = res.totalElements;

                    this.overdueSuppliers =
                        this.rows.filter(
                            row => row.hasOverdue
                        ).length;

                    this.highRiskSuppliers =
                        this.rows.filter(
                            row => row.riskLevel === 'HIGH'
                        ).length;
                },
                error: err => {

                    this.failure(
                        err?.message
                        || 'Failed to load supplier debt'
                    );

                }
            });
    }

    changePage(
        event: PageEvent
    ): void {

        this.query.page =
            event.pageIndex;

        this.query.size =
            event.pageSize;

        this.syncQuery();
    }

    sort(
        sortBy: string
    ): void {

        if (this.query.sortBy === sortBy) {

            this.query.direction =
                this.query.direction === 'asc'
                    ? 'desc'
                    : 'asc';

        } else {

            this.query.sortBy = sortBy;
            this.query.direction = 'desc';
        }

        this.syncQuery();
    }

    openWorkspace(
        row: SupplierDebtSummary
    ): void {

        this.router.navigate([
            '/app/finance/ap/debts',
            row.supplierId
        ]);
    }

    riskClass(
        level: string
    ): string {

        switch (level) {

            case 'HIGH':
                return 'risk-high';

            case 'MEDIUM':
                return 'risk-medium';

            case 'LOW':
                return 'risk-low';

            case 'GOOD':
                return 'risk-good';

            default:
                return 'risk-clear';
        }
    }

    private success(
        message: string
    ): void {

        this.snack.open(
            message,
            'Close',
            {
                duration: 5000,
                panelClass: [
                    'snackbar-success'
                ]
            }
        );
    }

    private failure(
        message: string
    ): void {

        this.snack.open(
            message,
            'Close',
            {
                duration: 8000,
                panelClass: [
                    'snackbar-error'
                ]
            }
        );
    }
}