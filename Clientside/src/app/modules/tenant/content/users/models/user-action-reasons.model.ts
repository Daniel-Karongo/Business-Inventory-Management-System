export enum UserActionType {
  DISABLE = 'DISABLE',
  RESTORE = 'RESTORE',
  DELETE = 'DELETE'
}

export const USER_ACTION_REASONS: Record<UserActionType, string[]> = {

  [UserActionType.DISABLE]: [
    'Security violation',
    'Policy breach',
    'Left organization',
    'Temporary suspension',
    'Access misuse detected'
  ],

  [UserActionType.RESTORE]: [
    'Mistaken disable',
    'Investigation cleared',
    'Reinstated by management'
  ],

  [UserActionType.DELETE]: [
    'Duplicate account',
    'Test user cleanup',
    'GDPR data removal'
  ]

};