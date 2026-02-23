export interface ChartPoint {
  label: string;
  value: number;
}

export interface AgingBreakdown {
  current: number;
  days30: number;
  days60: number;
  days90: number;
  over90: number;
}

export interface FinancialKPIs {
  netRevenueToday: number;
  grossProfitToday: number;
  vatPayable: number;
  accountsReceivable: number;
  accountsPayable: number;
  cashBalance: number;
  inventoryValue: number;
  corporateTaxAccrued: number;
  grossMarginPercent: number;
  inventoryTurnover: number;
  burnRate: number;

  arAging: AgingBreakdown;
  apAging: AgingBreakdown;

  revenueBudgetVariance: number;
  expenseBudgetVariance: number;
}

export interface OperationalKPIs {
  salesCountToday: number;
  refundCountToday: number;
  lowStockCount: number;
  outOfStockCount: number;
  deadStockValue: number;
}

export interface DashboardSummary {
  branchId: string;
  date: string;
  financial: FinancialKPIs;
  operational: OperationalKPIs;
  revenueTrend: ChartPoint[];
  profitTrend: ChartPoint[];
  vatTrend: ChartPoint[];
  topBatches: ChartPoint[];
  recentActivities: {
    type: string;
    description: string;
    actor?: string;
    time: string;
  }[];
}