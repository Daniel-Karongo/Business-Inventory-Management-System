import { SupplierBill } from './supplier-bill.model';
import { SupplierPayment } from './supplier-payment.model';
import { SupplierTimelineEntry } from './supplier-timeline-entry.model';

export interface SupplierWorkspace {
    supplierId: string;
    supplierName: string;
    totalOutstanding: number;
    overdueAmount: number;
    unappliedPayments: number;
    netPayable: number;
    openBills: number;
    overdueBills: number;
    bills: SupplierBill[];
    payments: SupplierPayment[];
    timeline: SupplierTimelineEntry[];
}