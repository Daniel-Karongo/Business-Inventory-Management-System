/**
 * Product action reasons used across the entire products module.
 * Centralized for maintainability and consistency.
 */

export enum ProductActionType {
  DISABLE = 'DISABLE',
  RESTORE = 'RESTORE',
  DELETE = 'DELETE'
}

export const PRODUCT_ACTION_REASONS: Record<ProductActionType, string[]> = {

  [ProductActionType.DISABLE]: [
    'Discontinued product',
    'Replaced by new model',
    'Incorrect configuration',
    'Pricing issue',
    'Supplier contract terminated'
  ],

  [ProductActionType.RESTORE]: [
    'Mistaken disable',
    'Supplier reinstated',
    'Pricing corrected',
    'Inventory discrepancy resolved'
  ],

  [ProductActionType.DELETE]: [
    'Duplicate product entry',
    'Created in error',
    'Test data cleanup',
    'Regulatory data cleanup'
  ]

};