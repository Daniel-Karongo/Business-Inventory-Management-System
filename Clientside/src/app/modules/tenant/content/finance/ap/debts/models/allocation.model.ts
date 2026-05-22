export interface AllocateSupplierPaymentRequest {
    branchId: string;
    supplierId: string;
    paymentId: string;
    purchaseInvoiceId: string;
    allocationAmount: number;
}

export interface AutoAllocatePaymentRequest {
    branchId: string;
    supplierId: string;
    paymentId: string;
    amount: number;
}

export interface AllocationPreviewItem {
    invoiceId: string;
    invoiceNumber: string;
    invoiceDate: string;
    dueDate: string;
    invoiceOutstanding: number;
    allocationAmount: number;
    remainingAfterAllocation: number;
}

export interface AllocationPreviewResponse {
    requestedAmount: number;
    totalAllocated: number;
    remainingUnallocated: number;
    allocations: AllocationPreviewItem[];
}

export interface AllocationResponse {
    allocationId: string;
    paymentId: string;
    paymentNumber: string;
    invoiceId: string;
    invoiceNumber: string;
    allocatedAmount: number;
    remainingInvoiceBalance: number;
    remainingPaymentBalance: number;
    allocatedAt: string;
    status: string;
    reversed: boolean;
    reversedAt?: string;
    reversedBy?: string;
    reversalReason?: string;
}