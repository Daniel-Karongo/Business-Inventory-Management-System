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
    Router,
    RouterModule
} from '@angular/router';

import {
    debounceTime,
    distinctUntilChanged,
    finalize
} from 'rxjs';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatCardModule
} from '@angular/material/card';

import {
    MatCheckboxModule
} from '@angular/material/checkbox';

import {
    MatDialog
} from '@angular/material/dialog';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatSnackBar
} from '@angular/material/snack-bar';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatTooltipModule
} from '@angular/material/tooltip';

import {
    OperationalExpense
} from '../../models/operational-expense.model';

import {
    OperationalExpenseService
} from '../../services/operational-expense.service';

import {
    BulkSettleExpensesDialogComponent
} from '../../components/bulk-settle-expenses-dialog/bulk-settle-expenses-dialog.component';

import {
    CreateExpenseDialogComponent
} from '../../components/create-expense-dialog/create-expense-dialog.component';
import { WorkflowShellComponent } from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { WorkflowCardComponent } from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

@Component({
    selector: 'app-operational-expense-list',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        RouterModule,
        CurrencyPipe,
        DatePipe,
        WorkflowShellComponent,
        WorkflowCardComponent,
        MatCardModule,
        MatButtonModule,
        MatIconModule,
        MatPaginatorModule,
        MatTableModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatCheckboxModule,
        MatTooltipModule
    ],
    templateUrl:
        './expense-list.component.html',
    styleUrls: [
        './expense-list.component.scss'
    ]
})
export class OperationalExpenseListComponent
    implements OnInit {

    private readonly service =
        inject(
            OperationalExpenseService
        );

    private readonly dialog =
        inject(
            MatDialog
        );

    private readonly snack =
        inject(
            MatSnackBar
        );

    private readonly router =
        inject(
            Router
        );

    loading = false;

    total = 0;

    rows:
        OperationalExpense[] = [];

    displayedColumns = [
        'select',
        'date',
        'description',
        'status',
        'amount',
        'settled',
        'actions'
    ];

    page = 0;

    size = 20;

    viewMode:
        'table'
        | 'grid' =
        localStorage.getItem(
            'expense-view'
        ) as any
        ||
        'table';

    density:
        'comfortable'
        | 'compact' =
        localStorage.getItem(
            'expense-density'
        ) as any
        ||
        'comfortable';

    selectedExpenseIds =
        new Set<string>();

    readonly searchControl =
        new FormControl(
            '',
            {
                nonNullable: true
            }
        );

    readonly statusControl =
        new FormControl(
            'ALL',
            {
                nonNullable: true
            }
        );

    ngOnInit(): void {

        this.searchControl
            .valueChanges
            .pipe(
                debounceTime(
                    300
                ),
                distinctUntilChanged()
            )
            .subscribe(
                () => {
                    this.page = 0;
                    this.load();
                }
            );

        this.statusControl
            .valueChanges
            .subscribe(
                () => {
                    this.page = 0;
                    this.load();
                }
            );

        this.load();
    }

    get openCount(): number {
        return this.rows.filter(
            x => x.status === 'OPEN'
        ).length;
    }

    get settledCount(): number {
        return this.rows.filter(
            x => x.status === 'SETTLED'
        ).length;
    }

    get hasSelection(): boolean {

        return (
            this.selectedExpenseIds
                .size > 0
        );
    }

    get selectedRows():
        OperationalExpense[] {

        return this.rows
            .filter(
                r =>
                    this.selectedExpenseIds
                        .has(
                            r.id
                        )
            );
    }

    load(): void {

        this.loading = true;

        const status =
            this.statusControl
                .value === 'ALL'
                ? undefined
                : this.statusControl
                    .value;

        this.service
            .list(
                this.page,
                this.size,
                this.searchControl.value,
                status
            )
            .pipe(
                finalize(
                    () =>
                        this.loading = false
                )
            )
            .subscribe({
                next: (res: any) => {
                    const page = res.data;

                    this.rows = page.content ?? [];
                    this.total = page.totalElements ?? 0;
                },
                error: err => {

                    this.snack.open(
                        err?.message
                        ||
                        'Failed to load expenses',
                        'Close',
                        {
                            duration:
                                5000
                        }
                    );
                }
            });
    }

    toggleSelection(
        row:
            OperationalExpense
    ): void {

        if (
            this.selectedExpenseIds
                .has(
                    row.id
                )
        ) {

            this.selectedExpenseIds
                .delete(
                    row.id
                );

            return;
        }

        this.selectedExpenseIds
            .add(
                row.id
            );
    }

    isSelected(
        expenseId: string
    ): boolean {

        return this.selectedExpenseIds
            .has(
                expenseId
            );
    }

    setViewMode(
        mode:
            'table'
            | 'grid'
    ): void {

        this.viewMode =
            mode;

        localStorage
            .setItem(
                'expense-view',
                mode
            );
    }

    toggleDensity(): void {

        this.density =
            this.density ===
                'comfortable'
                ? 'compact'
                : 'comfortable';

        localStorage
            .setItem(
                'expense-density',
                this.density
            );
    }

    createExpense(): void {

        this.dialog.open(
            CreateExpenseDialogComponent,
            {
                width: '720px',
                maxWidth: '96vw',
                maxHeight: '92vh',
                height: 'auto',
                autoFocus: false,
                panelClass: 'enterprise-dialog'
            }
        )
            .afterClosed()
            .subscribe(
                refresh => {

                    if (
                        !refresh
                    ) {
                        return;
                    }

                    this.load();

                    this.snack.open(
                        'Expense created',
                        'Close',
                        {
                            duration:
                                3000
                        }
                    );
                }
            );
    }

    bulkSettle(): void {

        this.dialog.open(
            BulkSettleExpensesDialogComponent,
            {
                width: '720px',
                maxWidth: '96vw',
                maxHeight: '92vh',
                height: 'auto',
                autoFocus: false,
                panelClass: 'enterprise-dialog',
                data: {
                    expenseIds:
                        this.selectedRows.map(
                            x => x.id
                        ),

                    expenseCount:
                        this.selectedRows.length,

                    totalOutstanding:
                        this.selectedRows.reduce(
                            (
                                total,
                                row
                            ) =>
                                total +
                                (
                                    row.amount -
                                    row.settledAmount
                                ),
                            0
                        )
                }
            }
        )
            .afterClosed()
            .subscribe(
                refresh => {

                    if (
                        !refresh
                    ) {
                        return;
                    }

                    this.selectedExpenseIds
                        .clear();

                    this.load();
                }
            );
    }

    openWorkspace(
        row:
            OperationalExpense
    ): void {

        this.router.navigate(
            [
                '/app/finance/expenses',
                row.id
            ]
        );
    }

    changePage(
        event:
            PageEvent
    ): void {

        this.page =
            event.pageIndex;

        this.size =
            event.pageSize;

        this.load();
    }

    outstanding(
        row:
            OperationalExpense
    ): number {

        return (
            row.amount
            -
            row.settledAmount
        );
    }

    statusClass(
        status: string
    ): string {

        switch (
        status
        ) {

            case 'OPEN':
                return 'badge-danger';

            case 'PARTIALLY_SETTLED':
                return 'badge-warn';

            case 'SETTLED':
                return 'badge-success';

            case 'REVERSED':
                return 'badge-neutral';

            default:
                return 'badge-neutral';
        }
    }
}