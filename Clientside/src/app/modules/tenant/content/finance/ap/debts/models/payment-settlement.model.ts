export type PaymentAllocationStatus =
    | 'ACTIVE'
    | 'REVERSED';

export interface PaymentSettlementDto {

    allocationId: string;

    invoiceId: string;

    billNumber: string;

    invoiceDate: string;

    allocatedAmount: number;

    status: PaymentAllocationStatus;

    reversed: boolean;

    reversedAt?: string;

    reversedBy?: string;

    reversalReason?: string;
}