export interface SupplierBillLine {
    productName: string;
    variantName: string;
    quantity: number;
    unitCost: number;
    totalCost: number;
}

export interface InvoiceSettlement {
    paymentId: string;
    paymentNumber: string;
    paymentDate: string;
    allocatedAmount: number;
    paymentMethod: string;
}

export interface SupplierBill {
    invoiceId: string;
    billNumber: string;
    invoiceDate: string;
    dueDate: string;
    totalAmount: number;
    paidAmount: number;
    remainingAmount: number;
    overdue: boolean;
    status: string;
    items: SupplierBillLine[];
    settlements: InvoiceSettlement[];
}