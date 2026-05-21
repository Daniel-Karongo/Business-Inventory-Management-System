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
    ActivatedRoute,
    Router
} from '@angular/router';

import {
    finalize
} from 'rxjs';

import {
    MatCardModule
} from '@angular/material/card';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatTabsModule
} from '@angular/material/tabs';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatDividerModule
} from '@angular/material/divider';

import {
    MatDialog
} from '@angular/material/dialog';

import {
    ApDebtService
} from '../../services/ap-debt.service';

import {
    SupplierWorkspace
} from '../../models/supplier-workspace.model';
import { WorkflowShellComponent } from '../../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { WorkflowCardComponent } from '../../../../../../../../shared/layout/workflow-card/workflow-card.component';
import {
    MatSnackBar
} from '@angular/material/snack-bar';

import {
    AllocationDialogComponent
} from '../../components/allocation-dialog/allocation-dialog.component';
import { BranchContextService } from '../../../../../../../../core/services/branch-context.service';
import { InvoiceDetailsDialogComponent } from '../../components/invoice-details-dialog/invoice-details-dialog.component';
import { PaymentDetailsDialogComponent } from '../../components/payment-details-dialog/payment-details-dialog.component';

@Component({
    selector: 'app-supplier-workspace',
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
    templateUrl: './supplier-workspace.component.html',
    styleUrls: ['./supplier-workspace.component.scss']
})
export class SupplierWorkspaceComponent
    implements OnInit {

    private route =
        inject(ActivatedRoute);

    private router =
        inject(Router);

    private debtService =
        inject(ApDebtService);

    readonly dialog =
        inject(MatDialog);

    private snackBar =
        inject(MatSnackBar);

    private branchContext =
        inject(BranchContextService);

    loading = false;

    workspace?: SupplierWorkspace;

    billColumns = [
        'invoice',
        'dueDate',
        'remaining',
        'status',
        'actions'
    ];

    paymentColumns = [
        'payment',
        'date',
        'amount',
        'unapplied',
        'actions'
    ];

    timelineColumns = [
        'date',
        'activity',
        'reference',
        'debit',
        'credit',
        'balance'
    ];

    expandedInvoiceId?: string;

    expandedPaymentId?: string;

    ngOnInit(): void {
        this.load();
    }

    load(): void {

        const supplierId =
            this.route.snapshot.paramMap.get(
                'supplierId'
            );

        if (!supplierId) {
            return;
        }

        this.loading = true;

        this.debtService
            .getWorkspace(supplierId)
            .pipe(
                finalize(() =>
                    this.loading = false
                )
            )
            .subscribe(res =>
                this.workspace = res
            );
    }

    trackByInvoice(
        _: number,
        row: any
    ): string {

        return row.invoiceId;
    }

    trackByPayment(
        _: number,
        row: any
    ): string {

        return row.paymentId;
    }

    openInvoiceDetails(
        bill: any,
        event?: Event
    ): void {

        event?.stopPropagation();

        this.dialog.open(
            InvoiceDetailsDialogComponent,
            {
                width: '900px',
                maxWidth: '95vw',
                data: bill
            }
        );
    }

    openPaymentDetails(
        payment: any,
        event?: Event
    ): void {

        event?.stopPropagation();

        this.dialog.open(
            PaymentDetailsDialogComponent,
            {
                width: '900px',
                maxWidth: '95vw',
                data: payment
            }
        );
    }

    toggleInvoice(
        invoiceId: string
    ): void {

        this.expandedInvoiceId =
            this.expandedInvoiceId === invoiceId
                ? undefined
                : invoiceId;
    }

    togglePayment(
        paymentId: string
    ): void {

        this.expandedPaymentId =
            this.expandedPaymentId === paymentId
                ? undefined
                : paymentId;
    }

    back(): void {

        this.router.navigate([
            '/app/finance/ap/debts'
        ]);
    }

    riskClass(
        level?: string
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

    openAllocationDialog(): void {

        if (!this.workspace) {
            return;
        }

        this.dialog.open(
            AllocationDialogComponent,
            {
                width: '760px',
                maxWidth: '95vw',
                data: {
                    branchId:
                        this.branchContext.currentBranch,

                    supplierId:
                        this.workspace.supplierId,

                    payments:
                        this.workspace.payments
                            .filter(p =>
                                p.unappliedAmount > 0
                            ),

                    bills:
                        this.workspace.bills
                            .filter(b =>
                                b.remainingAmount > 0
                            )
                }
            }
        )
            .afterClosed()
            .subscribe(refresh => {

                if (!refresh) {
                    return;
                }

                this.load();

                this.snackBar.open(
                    'Workspace refreshed',
                    'Close',
                    { duration: 2500 }
                );
            });
    }
}