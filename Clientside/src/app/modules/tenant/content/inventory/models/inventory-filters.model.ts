export interface InventoryFilters {
  branchId?: string;
  productName?: string;
  classification?: string;
  stockState?: 'ALL' | 'LOW' | 'OUT';
}