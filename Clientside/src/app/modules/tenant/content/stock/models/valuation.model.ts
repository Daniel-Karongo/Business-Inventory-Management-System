export interface InventoryValuationDashboard {
    valuationMethod: string;
    totalValuation: number;
    branchValuation: unknown;
    categoryValuation: unknown;
    topProducts: unknown[];
}

export interface HistoricalInventoryValuation {
    snapshotDate?: string;
    valuationMethod?: string;
    totalValuation?: number;
    items?: unknown[];
}