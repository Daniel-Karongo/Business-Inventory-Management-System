import { SupplierBill } from './supplier-bill.model';
import { SupplierTimelineEntryDto } from './supplier-timeline-entry.model';
import { PaymentSettlementDto } from './payment-settlement.model';

export type WorkspacePaymentMethod =
    | 'CASH'
    | 'BANK'
    | 'MPESA';

export type WorkspacePaymentStatus =
    | 'DRAFT'
    | 'APPROVED'
    | 'POSTED'
    | 'PARTIALLY_ALLOCATED'
    | 'FULLY_ALLOCATED'
    | 'REVERSED'
    | 'CANCELLED';

export type FinancialPostingStatus =
    | 'UNPOSTED'
    | 'POSTED'
    | 'PARTIAL'
    | 'REVERSED';

export interface SupplierWorkspacePaymentDto {

    paymentId: string;

    paymentNumber: string;

    paymentDate: string;

    amount: number;

    allocatedAmount: number;

    unappliedAmount: number;

    status: WorkspacePaymentStatus;

    paymentMethod: WorkspacePaymentMethod;

    reference?: string;

    posted: boolean;

    reversed: boolean;

    postingStatus: FinancialPostingStatus;

    allocations: PaymentSettlementDto[];
}

export interface SupplierWorkspaceDto {

    supplierId: string;

    supplierName: string;

    totalOutstanding: number;

    overdueAmount: number;

    unappliedPayments: number;

    netPayable: number;

    openBills: number;

    overdueBills: number;

    bills: SupplierBill[];

    payments: SupplierWorkspacePaymentDto[];

    timeline: SupplierTimelineEntryDto[];
}