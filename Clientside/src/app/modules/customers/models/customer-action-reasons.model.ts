export enum CustomerActionType {
  DISABLE = 'DISABLE',
  RESTORE = 'RESTORE',
  DELETE = 'DELETE'
}

export const CUSTOMER_ACTION_REASONS: Record<CustomerActionType, string[]> = {

  [CustomerActionType.DISABLE]: [
    'Fraud suspicion',
    'Policy violation',
    'Requested by management'
  ],

  [CustomerActionType.RESTORE]: [
    'Mistaken disable',
    'Issue resolved'
  ],

  [CustomerActionType.DELETE]: [
    'Duplicate record',
    'GDPR removal request',
    'Test data cleanup'
  ]

};