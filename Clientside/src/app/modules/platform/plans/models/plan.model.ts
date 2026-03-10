export interface PlanResponse {

  id: string;

  code: string;

  name: string;

  maxUsers: number;

  maxBranches: number;

  inventoryEnabled: boolean;

  accountingEnabled: boolean;

  reportingEnabled: boolean;

  requestsPerMinute: number;

}

export interface PlanCreateRequest {

  code: string;

  name: string;

  maxUsers: number;

  maxBranches: number;

  inventoryEnabled: boolean;

  accountingEnabled: boolean;

  reportingEnabled: boolean;

  requestsPerMinute: number;

}