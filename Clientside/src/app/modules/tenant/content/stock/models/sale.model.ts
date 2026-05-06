import { BatchConsumptionDTO } from './inventory-batch.model';

export type SaleStatus =
    | 'CREATED'
    | 'COMPLETED'
    | 'CANCELLED'
    | 'REFUNDED';

export interface PaymentDTO {
    id?: string;
    amount: number;
    paymentMethod?: string;
    reference?: string;
    paidAt?: string;
    status?: string;
}

export interface SaleCustomerDTO {
    id: string;
    name: string;
    phoneNumbers: string[];
    emailAddresses: string[];
}

export interface SaleLineItemDTO {
    productVariantId: string;
    productName: string;
    branchId: string;
    quantity: number;
    unitPrice: number;
    lineTotal: number;
    batchConsumptions: BatchConsumptionDTO[];
}

export interface SaleDTO {
    id: string;
    receiptNo: string;
    createdAt: string;
    createdBy: string;
    totalAmount: number;
    totalTax: number;
    totalDiscount: number;
    status: SaleStatus;
    customerId?: string;
    items: SaleLineItemDTO[];
    payments: PaymentDTO[];
    customer?: SaleCustomerDTO;
}

export interface BatchSelectionDto {
    batchId: string;
    quantity: number;
}

export interface SaleLineDto {
    productVariantId: string;
    branchId: string;
    quantity: number;
    unitPrice?: number;
    batchSelections?: BatchSelectionDto[];
    packagingId: string;
}

export interface CustomerRequest {
    customerId?: string;
    customerGroupId?: string;
    customerName?: string;
    phoneNumber?: string;
    email?: string;
}

export interface SaleRequest {
    items: SaleLineDto[];
    payments: PaymentDTO[];
    totalAmount: number;
    totalTax?: number;
    totalDiscount?: number;
    customerIdentifiers?: CustomerRequest;
    reference?: string;
    overrideMinimumPrice?: boolean;
    overrideReason?: string;
}

export interface SaleResponse {
    saleId: string;
    createdAt: string;
    createdBy: string;
    status: SaleStatus;
    summary?: unknown;
}

export interface SaleBulkPreviewRow {
    receiptNo: string;
    productName: string;
    sku: string;
    quantity: number;
    unitPrice: number;
    branchCode: string;
}

export interface SaleBulkRow {
    receiptNo: string;
    sku?: string;
    productName?: string;
    packagingId?: string;
    customerId?: string;
    customerGroupId?: string;
    variant?: string;
    quantity: number;
    unitPrice: number;
    branchCode?: string;
    saleDate?: string;
    payments?: string;
}