export enum SupplierActionType {
  DISABLE = 'DISABLE',
  RESTORE = 'RESTORE',
  DELETE = 'DELETE'
}

export const SUPPLIER_ACTION_REASONS: Record<SupplierActionType, string[]> = {

  [SupplierActionType.DISABLE]: [
    'Contract terminated',
    'Compliance issue',
    'Low performance rating'
  ],

  [SupplierActionType.RESTORE]: [
    'Contract renewed',
    'Compliance resolved'
  ],

  [SupplierActionType.DELETE]: [
    'Duplicate supplier',
    'Fraudulent record',
    'Test data cleanup',
    'Regulatory removal request'
  ]

};