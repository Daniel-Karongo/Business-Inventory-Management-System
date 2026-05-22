import { SupplierPayment } from './supplier-payment.model';
export interface SupplierPaymentDetails {
    payment: SupplierPayment;
    allocations: {
        invoiceId: string;
        billNumber: string;
        invoiceDate: string;
        allocatedAmount: number;
    }[];
}