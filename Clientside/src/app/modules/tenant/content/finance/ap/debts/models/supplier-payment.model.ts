export interface PaymentSettlement {
    allocationId?: string;
    invoiceId: string;
    billNumber: string;
    invoiceDate: string;
    allocatedAmount: number;
    status?: string;
    reversed?: boolean;
    reversedAt?: string;
    reversedBy?: string;
    reversalReason?: string;
}

export interface SupplierPayment {
    paymentId: string;
    paymentNumber: string;
    paymentDate: string;
    amount: number;
    allocatedAmount: number;
    unappliedAmount: number;
    paymentMethod: string;
    fundingAccountId?: string;
    reference: string;
    allocations: PaymentSettlement[];
    status: string;
    posted: boolean;
    postingStatus: string;
    documentStatus: string;
    fullyAllocated: boolean;
    reversed: boolean;
    reversedAt?: string;
    reversedBy?: string;
    reversalReason?: string;
}