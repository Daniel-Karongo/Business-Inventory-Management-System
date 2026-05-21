export interface SupplierDebtSummary {
  supplierId: string;
  supplierName: string;

  totalOutstanding: number;
  overdueAmount: number;
  unappliedPayments: number;
  netPayable: number;

  openBills: number;
  overdueBills: number;

  oldestDueDate: string | null;
  lastPaymentDate: string | null;

  riskLevel: 'CLEAR' | 'GOOD' | 'LOW' | 'MEDIUM' | 'HIGH';

  hasOverdue: boolean;
  hasUnappliedPayments: boolean;
}