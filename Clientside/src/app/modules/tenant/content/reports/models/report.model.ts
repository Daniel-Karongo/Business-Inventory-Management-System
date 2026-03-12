export type ReportType =
  | 'trial_balance'
  | 'profit_and_loss'
  | 'general_ledger'
  | 'sales_summary';

export interface ReportDefinition {
  key: ReportType;
  title: string;
  description: string;
  requiresAccount?: boolean;
}