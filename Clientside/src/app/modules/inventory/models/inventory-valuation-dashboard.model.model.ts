export interface InventoryValuationDashboard {
  valuationMethod: string;
  totalValuation: number;

  branchValuation: Record<string, number>;

  categoryValuation: {
    valuationMethod: string;
    categories: Record<string, number>;
  };

  topProducts: {
    productId: string;
    valuation: number;
  }[];
}