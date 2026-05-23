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
    SupplierWorkspaceDto,
    SupplierWorkspacePaymentDto
} from '../../models/supplier-workspace.model';
import { SupplierBill } from '../../models/supplier-bill.model';
import { PaymentSettlementDto } from '../../models/payment-settlement.model';
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
import { CreatePaymentDialogComponent } from '../../components/create-payment-dialog/create-payment-dialog.component';
import { ApPaymentService } from '../../services/ap-payment.service';
import { ApAllocationService } from '../../services/ap-allocation.service';
import { ReasonDialogComponent } from '../../../../../../../../shared/components/reason-dialog/reason-dialog.component';

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

    private paymentService =
        inject(ApPaymentService);

    private allocationService =
        inject(ApAllocationService);

    loading = false;
    postingPayments = new Set<string>();
    reversingPayments = new Set<string>();
    reversingAllocations = new Set<string>();
    workspace?: SupplierWorkspaceDto;

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
        'status',
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
        row: SupplierBill
    ): string {

        return row.invoiceId;
    }

    trackByPayment(
        _: number,
        row: SupplierWorkspacePaymentDto
    ): string {

        return row.paymentId;
    }

    openInvoiceDetails(
        bill: SupplierBill,
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

    paymentBadgeClass(
        status: string
    ): string {

        switch (status) {

            case 'DRAFT':
                return 'badge-warn';

            case 'PARTIALLY_ALLOCATED':
                return 'badge-info';

            case 'FULLY_ALLOCATED':
                return 'badge-success';

            case 'REVERSED':
                return 'badge-danger';

            case 'POSTED':
                return 'badge-primary';

            default:
                return 'badge-neutral';
        }
    }

    postingLabel(
        payment: SupplierWorkspacePaymentDto
    ): string {

        if (payment.reversed) {
            return 'Reversed';
        }

        switch (payment.postingStatus) {

            case 'POSTED':
                return 'Posted';

            case 'REVERSED':
                return 'Reversed';

            default:
                return 'Draft';
        }
    }

    openPaymentDetails(
        payment: SupplierWorkspacePaymentDto,
        event?: Event
    ): void {

        event?.stopPropagation();

        this.paymentService
            .details(payment.paymentId)
            .subscribe({

                next: details => {

                    this.dialog.open(
                        PaymentDetailsDialogComponent,
                        {
                            width: '820px',

                            maxWidth: '95vw',

                            panelClass: 'payment-details-dialog-panel',

                            autoFocus: false,

                            data: details
                        }
                    );
                },

                error: err => {

                    this.snackBar.open(
                        err?.error?.message
                        ||
                        'Failed to load payment details',
                        'Close',
                        {
                            duration: 5000
                        }
                    );
                }
            });
    }

    postPayment(
        payment: SupplierWorkspacePaymentDto
    ): void {

        if (
            payment.posted
            ||
            payment.reversed
            ||
            this.postingPayments.has(payment.paymentId)
        ) return;

        this.postingPayments.add(payment.paymentId);

        this.paymentService
            .post(payment.paymentId)
            .pipe(
                finalize(() =>
                    this.postingPayments.delete(payment.paymentId)
                )
            )
            .subscribe({

                next: () => {

                    this.snackBar.open(
                        'Payment posted',
                        'Close',
                        { duration: 2500 }
                    );

                    this.load();
                },

                error: err => {

                    this.snackBar.open(
                        err?.message
                        ||
                        'Posting failed',
                        'Close',
                        { duration: 5000 }
                    );

                    this.load();
                }
            });
    }

    reversePayment(
        payment: SupplierWorkspacePaymentDto
    ): void {

        if (
            payment.reversed
            ||
            this.reversingPayments.has(payment.paymentId)
        ) return;

        this.dialog.open(
            ReasonDialogComponent,
            {
                width: '520px',
                maxWidth: '95vw',
                data: {
                    title: 'Reverse Payment',
                    message: `Reverse payment ${payment.paymentNumber}?`,
                    confirmText: 'Reverse',
                    reasons: [
                        'Duplicate payment',
                        'Wrong supplier',
                        'Wrong amount',
                        'Fraud suspected',
                        'Bank rejection'
                    ],
                    allowCustomReason: true,
                    requireReason: true
                }
            }
        )
            .afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) return;

                const reason =
                    result.reason;

                this.reversingPayments.add(
                    payment.paymentId
                );

                this.paymentService
                    .reverse(payment.paymentId,
                        { reason }
                    )
                    .pipe(
                        finalize(() =>
                            this.reversingPayments.delete(
                                payment.paymentId
                            )
                        )
                    )
                    .subscribe({

                        next: () => {

                            this.snackBar.open(
                                'Payment reversed',
                                'Close',
                                { duration: 3000 }
                            );

                            this.load();
                        },

                        error: err => {

                            this.snackBar.open(
                                err?.message
                                ||
                                'Reversal failed',
                                'Close',
                                { duration: 5000 }
                            );

                            this.load();
                        }
                    });
            });
    }

    reverseAllocation(
        allocation: PaymentSettlementDto
    ): void {

        if (
            allocation.reversed
            ||
            !allocation.allocationId
            ||
            this.reversingAllocations.has(
                allocation.allocationId
            )
        ) return;

        this.dialog.open(
            ReasonDialogComponent,
            {
                width: '520px',
                maxWidth: '95vw',
                data: {
                    title: 'Reverse Allocation',
                    message: `Reverse allocation for ${allocation.billNumber}?`,
                    confirmText: 'Reverse',
                    reasons: [
                        'Incorrect allocation',
                        'Duplicate allocation',
                        'Invoice dispute',
                        'Supplier reconciliation'
                    ],
                    allowCustomReason: true,
                    requireReason: true
                }
            }
        )
            .afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) return;

                const reason =
                    result.reason;

                this.reversingAllocations.add(
                    allocation.allocationId
                );

                this.allocationService
                    .reverse(
                        allocation.allocationId,
                        reason
                    )
                    .pipe(
                        finalize(() =>
                            this.reversingAllocations.delete(
                                allocation.allocationId
                            )
                        )
                    )
                    .subscribe({

                        next: () => {

                            this.snackBar.open(
                                'Allocation reversed',
                                'Close',
                                { duration: 3000 }
                            );

                            this.load();
                        },

                        error: err => {

                            this.snackBar.open(
                                err?.message
                                ||
                                'Allocation reversal failed',
                                'Close',
                                { duration: 5000 }
                            );

                            this.load();
                        }
                    });
            });
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

    openCreatePaymentDialog(): void {

        if (!this.workspace) return;

        this.dialog.open(
            CreatePaymentDialogComponent,
            {
                width: '720px',
                maxWidth: '96vw',
                maxHeight: '92vh',
                autoFocus: false,
                panelClass: 'enterprise-dialog',
                data: {
                    branchId: this.branchContext.currentBranch,
                    supplierId: this.workspace.supplierId
                }
            }
        )
            .afterClosed()
            .subscribe(refresh => {

                if (!refresh) return;

                this.load();

                this.snackBar.open(
                    'Payment made successfully',
                    'Close',
                    { duration: 2500 }
                );
            });
    }

    openAllocationDialog(): void {

        if (!this.workspace) {
            return;
        }

        this.dialog.open(
            AllocationDialogComponent,
            {
                width: '720px',
                maxWidth: '96vw',
                maxHeight: '92vh',
                autoFocus: false,
                panelClass: 'enterprise-dialog',
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
                    'Allocation made successfully',
                    'Close',
                    { duration: 2500 }
                );
            });
    }
}