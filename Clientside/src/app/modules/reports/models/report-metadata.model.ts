export type ReportParamType =
  | 'DATE'
  | 'BRANCH'
  | 'ACCOUNT'
  | 'CUSTOMER'
  | 'SUPPLIER'
  | 'PRODUCT'
  | 'CATEGORY'
  | 'VARIANT'
  | 'NUMBER'
  | 'TEXT';

export interface ReportParameter {
  name: string;
  required: boolean;
  type: ReportParamType;
}

export interface ReportMetadata {
  key: string;
  title: string;
  description: string;
  category: string;

  parameters: ReportParameter[];

  minimumRole: string;

  supportsPdf: boolean;
  supportsXlsx: boolean;
  supportsCsv: boolean;

  maxDateRange?: number;
  dateRangeUnit?: string;
}