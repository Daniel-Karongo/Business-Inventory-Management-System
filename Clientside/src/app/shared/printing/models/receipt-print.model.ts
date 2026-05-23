export interface ReceiptPrintItem {
    productName: string;
    variantName?: string;
    quantity: number;
    unitPrice: number;
    total: number;
}

export interface ReceiptPrintPayment {
    method: string;
    amount: number;
    reference?: string;
    status?: string;
    timestamp?: string;
}

export interface ReceiptPrintCustomer {
    name?: string;
    phone?: string;
    email?: string;
}

export interface ReceiptPrintTotals {
    subtotal: number;
    tax: number;
    discount: number;
    total: number;
}

export interface ReceiptPrintData {
    receiptNumber: string;
    invoiceNumber?: string;

    createdAt: string;
    servedBy?: string;

    branchName?: string;
    branchAddress?: string;
    branchPhone?: string;
    branchEmail?: string;

    customer?: ReceiptPrintCustomer;

    items: ReceiptPrintItem[];

    totals: ReceiptPrintTotals;

    payments?: ReceiptPrintPayment[];

    footerText?: string;

    qrCodeValue?: string;
    barcodeValue?: string;
}