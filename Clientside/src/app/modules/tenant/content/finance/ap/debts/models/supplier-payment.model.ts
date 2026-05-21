export interface PaymentSettlement {
    invoiceId: string;
    billNumber: string;
    invoiceDate: string;
    allocatedAmount: number;
}

export interface SupplierPayment {
    paymentId: string;
    paymentNumber: string;
    paymentDate: string;
    amount: number;
    allocatedAmount: number;
    unappliedAmount: number;
    paymentMethod: string;
    reference: string;
    allocations: PaymentSettlement[];
}