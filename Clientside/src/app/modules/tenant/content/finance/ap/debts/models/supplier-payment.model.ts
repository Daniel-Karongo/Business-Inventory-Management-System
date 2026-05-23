export type SupplierPaymentStatus =
    | 'DRAFT'
    | 'APPROVED'
    | 'POSTED'
    | 'PARTIALLY_ALLOCATED'
    | 'FULLY_ALLOCATED'
    | 'REVERSED'
    | 'CANCELLED';

export type SupplierPaymentMethod =
    | 'CASH'
    | 'BANK'
    | 'MPESA';

export interface SupplierPaymentResponseDto {
    id: string;
    documentNumber: string;

    supplierId: string;
    fundingAccountId: string;

    amount: number;
    allocatedAmount: number;
    unappliedAmount: number;

    fullyAllocated: boolean;

    status: SupplierPaymentStatus;
    method: SupplierPaymentMethod;

    reference?: string;

    posted: boolean;

    paymentDate: string;

    postedAt?: string;
    postedBy?: string;

    reversed: boolean;

    reversedAt?: string;
    reversedBy?: string;
    reversalReason?: string;
}

export interface ProcessSupplierPaymentRequest {
    branchId: string;
    supplierId: string;
    fundingAccountId: string;

    amount: number;

    method: SupplierPaymentMethod;

    reference?: string;

    paymentDate: string;

    autoPost: boolean;
    autoAllocate: boolean;
}