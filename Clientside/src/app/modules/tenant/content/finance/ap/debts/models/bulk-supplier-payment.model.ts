export interface BulkSupplierPaymentRequest {
    branchId: string;
    supplierIds: string[];
    fundingAccountId: string;
    method: 'CASH' | 'BANK' | 'MPESA';
    reference?: string;
    paymentDate: string;
    autoAllocate: boolean;
}

export interface BulkSupplierPaymentResult {
    supplierId: string;
    supplierName: string;
    amountPaid: number;
    status: string;
    message?: string;
}