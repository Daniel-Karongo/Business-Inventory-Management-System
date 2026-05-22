export interface CreateSupplierPaymentRequest {
    branchId: string;
    supplierId: string;
    fundingAccountId: string;
    amount: number;
    method: 'CASH' | 'BANK' | 'MPESA';
    reference?: string;
    paymentDate: string;
}

export interface ReverseSupplierPaymentRequest {
    reason: string;
}