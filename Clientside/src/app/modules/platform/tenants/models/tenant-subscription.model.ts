export interface TenantSubscription {

  tenantId: string;

  planId: string;

  planName: string;

  priceMonthly: number;

  priceYearly: number;

  maxUsers: number;

  maxBranches: number;

  maxProducts: number;

  active: boolean;

  startDate: string;

}