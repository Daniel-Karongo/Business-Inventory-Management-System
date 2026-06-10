import { CommonModule, CurrencyPipe, DatePipe } from '@angular/common';
import { Component, OnInit, inject } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { finalize } from 'rxjs';

import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatDialog } from '@angular/material/dialog';
import { MatDividerModule } from '@angular/material/divider';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTableModule } from '@angular/material/table';
import { MatTabsModule } from '@angular/material/tabs';

import { WorkflowShellComponent } from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { WorkflowCardComponent } from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import { OperationalExpenseWorkspace } from '../../models/operational-expense-workspace.model';
import { OperationalExpenseSettlement } from '../../models/operational-expense.model';

import { OperationalExpenseService } from '../../services/operational-expense.service';

import { ReasonDialogComponent } from '../../../../../../../shared/components/reason-dialog/reason-dialog.component';
import { SettleExpenseDialogComponent } from '../../components/settle-expense-dialog/settle-expense-dialog.component';

@Component({
    selector: 'app-expense-workspace',
    standalone: true,
    imports: [
        CommonModule,
        CurrencyPipe,
        DatePipe,
        WorkflowShellComponent,
        WorkflowCardComponent,
        MatCardModule,
        MatButtonModule,
        MatIconModule,
        MatTabsModule,
        MatTableModule,
        MatDividerModule
    ],
    templateUrl: './expense-workspace.component.html',
    styleUrls: ['./expense-workspace.component.scss']
})
export class ExpenseWorkspaceComponent implements OnInit {

    private readonly route = inject(ActivatedRoute);
    private readonly router = inject(Router);
    private readonly service = inject(OperationalExpenseService);
    private readonly dialog = inject(MatDialog);
    private readonly snack = inject(MatSnackBar);

    loading = false;

    reversingExpense = false;

    reversingSettlements =
        new Set<string>();

    workspace?: OperationalExpenseWorkspace;

    settlementColumns = [
        'date',
        'reference',
        'amount',
        'status',
        'actions'
    ];

    ngOnInit(): void {
        this.load();
    }

    get expense() {
        return this.workspace?.expense;
    }

    get outstandingAmount(): number {

        if (!this.expense) {
            return 0;
        }

        return (
            this.expense.amount -
            this.expense.settledAmount
        );
    }

    get activeSettlementTotal(): number {

        return (
            this.workspace?.settlements
                .filter(s => !s.reversed)
                .reduce(
                    (a, b) => a + b.amount,
                    0
                ) ?? 0
        );

    }

    get reversedSettlementTotal(): number {

        return (
            this.workspace?.settlements
                .filter(s => s.reversed)
                .reduce(
                    (a, b) => a + b.amount,
                    0
                ) ?? 0
        );

    }

    get activeSettlementCount(): number {
        return (
            this.workspace?.settlements
                .filter(s => !s.reversed)
                .length ?? 0
        );
    }

    get reversedSettlementCount(): number {
        return (
            this.workspace?.settlements
                .filter(s => s.reversed)
                .length ?? 0
        );
    }

    load(): void {

        const expenseId =
            this.route.snapshot.paramMap.get(
                'expenseId'
            );

        if (!expenseId) {
            return;
        }

        this.loading = true;

        this.service
            .workspace(expenseId)
            .pipe(
                finalize(() =>
                    this.loading = false
                )
            )
            .subscribe({
                next: (res: any) => {
                    this.workspace = res.data;
                },
                error: err => {
                    this.snack.open(
                        err?.message || 'Failed to load workspace',
                        'Close',
                        { duration: 5000 }
                    );
                }
            });
    }

    back(): void {

        this.router.navigate([
            '/app/finance/expenses'
        ]);
    }

    statusClass(
        status: string
    ): string {

        switch (status) {

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

    reverseExpense(): void {

        if (
            !this.workspace ||
            this.reversingExpense ||
            this.expense?.status === 'REVERSED'
        ) {
            return;
        }

        this.dialog.open(
            ReasonDialogComponent,
            {
                width: '520px',
                maxWidth: '95vw',
                panelClass: 'enterprise-dialog',
                data: {
                    title: 'Reverse Expense',
                    action: 'DELETE',
                    message: 'Reverse this expense?',
                    confirmText: 'Reverse',
                    requireReason: true,
                    allowCustomReason: true,
                    reasons: [
                        'Duplicate expense entry',
                        'Incorrect amount recorded',
                        'Wrong expense account selected',
                        'Expense entered in error',
                        'Expense belongs to another transaction',
                        'Expense no longer applicable',
                        'Source document cancelled',
                        'Supplier transaction reversed',
                        'Correction of accounting error',
                        'Management approval withdrawn'
                    ]
                }
            }
        )
            .afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) {
                    return;
                }

                this.reversingExpense = true;

                this.service
                    .reverseExpense(
                        this.workspace!.expenseId,
                        result.reason
                    )
                    .pipe(
                        finalize(() =>
                            this.reversingExpense = false
                        )
                    )
                    .subscribe({
                        next: () => {

                            this.snack.open(
                                'Expense reversed',
                                'Close',
                                { duration: 3000 }
                            );

                            this.load();
                        }
                    });
            });
    }

    reverseSettlement(
        settlement: OperationalExpenseSettlement
    ): void {

        if (
            settlement.reversed ||
            this.reversingSettlements.has(
                settlement.id
            )
        ) {
            return;
        }

        this.dialog.open(
            ReasonDialogComponent,
            {
                width: '520px',
                maxWidth: '95vw',
                panelClass: 'enterprise-dialog',
                data: {
                    title: 'Reverse Settlement',
                    message: 'Reverse settlement?',
                    confirmText: 'Reverse',
                    requireReason: true,
                    allowCustomReason: true,
                    reasons: [
                        'Duplicate settlement',
                        'Incorrect settlement amount',
                        'Wrong funding account used',
                        'Settlement posted in error',
                        'Bank transaction failed',
                        'Cheque dishonoured',
                        'Payment reversed by bank',
                        'Settlement allocated incorrectly',
                        'Correction of accounting error',
                        'Management approval withdrawn'
                    ]
                }
            }
        )
            .afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) {
                    return;
                }

                this.reversingSettlements.add(
                    settlement.id
                );

                this.service
                    .reverseSettlement(
                        settlement.id,
                        result.reason
                    )
                    .pipe(
                        finalize(() =>
                            this.reversingSettlements.delete(
                                settlement.id
                            )
                        )
                    )
                    .subscribe({
                        next: () => {

                            this.snack.open(
                                'Settlement reversed',
                                'Close',
                                { duration: 3000 }
                            );

                            this.load();
                        }
                    });
            });
    }

    openSettlementDialog(): void {

        if (!this.workspace || !this.expense) {
            return;
        }

        this.dialog.open(
            SettleExpenseDialogComponent,
            {
                width: '720px',
                maxWidth: '96vw',
                maxHeight: '92vh',
                height: 'auto',
                autoFocus: false,
                panelClass: 'enterprise-dialog',
                data: {
                    expenseId: this.expense.id,
                    description: this.expense.description,
                    amount: this.expense.amount,
                    settledAmount: this.expense.settledAmount,
                    outstandingAmount: this.outstandingAmount
                }
            }
        )
            .afterClosed()
            .subscribe(refresh => {

                if (!refresh) {
                    return;
                }

                this.load();

                this.snack.open(
                    'Expense settled successfully',
                    'Close',
                    { duration: 3000 }
                );

            });

    }
}