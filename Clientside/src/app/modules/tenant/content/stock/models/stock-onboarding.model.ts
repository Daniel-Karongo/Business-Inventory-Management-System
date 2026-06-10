export interface PackagingInput {
  name: string;
  units: number;
}

export interface PricingInput {
  packagingName: string;
  sellingPrice: number;
}

export interface SupplierInput {
  supplierId?: string;
  supplierName?: string;
  packagingId?: string;
  packagingName?: string;
  unitsSupplied: number;
  unitCost: number;
  vatInclusive: boolean;
  vatRate: number;
}

export type SupplierPaymentMethod =
  | 'CASH'
  | 'BANK'
  | 'MPESA';
export interface OperationalExpenseInput {
  expenseAccountId: string;
  description: string;
  amount: number;
}

export interface StockOnboardingRequest {
  productName?: string;
  productId?: string;
  categoryId?: number;
  newCategoryName?: string;
  createCategoryIfMissing?: boolean;
  createProductIfMissing?: boolean;
  supplierIds?: string[];
  minimumPercentageProfit?: number;
  classification?: string;
  variantId?: string;
  packagings: PackagingInput[];
  pricing: PricingInput[];
  suppliers: SupplierInput[];
  branchId: string;
  reference?: string;
  note?: string;
  accountingDate: string;
  operationalExpenses?: OperationalExpenseInput[];
  autoPaySuppliers?: boolean;
  supplierPaymentMethod?: SupplierPaymentMethod;
  autoPayOperationalExpenses?: boolean;
  fundingAccountId?: string;
}

export interface BulkStockOnboardingRequest {
  rows: StockOnboardingRequest[];
  operationalExpenses?: OperationalExpenseInput[];
  autoPayOperationalExpenses?: boolean;
  fundingAccountId?: string;
}

export interface StockOnboardingResponse {
  productId: string;
  variantId: string;
  branchId: string;
  totalUnitsReceived: number;
  totalCost: number;
  message: string;
}
/* =====================================================
   BULK PREVIEW
===================================================== */
export interface StockOnboardingBulkPreviewRow {
  productId?: string;
  productName?: string;
  variantId?: string;
  classification?: string;
  branchId: string;
  totalUnits: number;
  grossCost: number;
  netCost: number;
  vatAmount: number;
  existingProduct: boolean;
  existingVariant: boolean;
  categoryCreated: boolean;
  suppliersCreated: number;
  packagingCount: number;
  pricingCount: number;
}

export interface StockOnboardingBulkPreviewResult {
  rows: StockOnboardingBulkPreviewRow[];
  totalUnits: number;
  grossCost: number;
  netCost: number;
  vatAmount: number;
}

export interface SupplierPreviewLine {
  supplierName: string;
  packagingName: string;
  unitsSupplied: number;
  quotedUnitCost: number;
  vatInclusive: boolean;
  vatRate: number;
  grossCost: number;
  netCost: number;
  vatAmount: number;
}

export interface StockOnboardingPreviewResponse {
  supplierLines: SupplierPreviewLine[];
  totalGrossCost: number;
  totalNetCost: number;
  totalVatAmount: number;
  totalUnits: number;
}

export interface PreviewSupplierLine {
  supplierName: string;
  packagingName: string;
  unitsSupplied: number;
  quotedUnitCost: number;
  vatInclusive: boolean;
  vatRate: number;
  grossCost: number;
  netCost: number;
  vatAmount: number;
}

export interface PreviewTotals {
  totalGrossCost: number;
  totalNetCost: number;
  totalVatAmount: number;
  totalUnits: number;
}

export interface PreviewResponse {
  supplierLines: PreviewSupplierLine[];
  totals: PreviewTotals;
}