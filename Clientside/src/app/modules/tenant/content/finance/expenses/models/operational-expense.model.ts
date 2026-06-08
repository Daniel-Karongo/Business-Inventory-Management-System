export type ExpenseStatus =
    | 'OPEN'
    | 'PARTIALLY_SETTLED'
    | 'SETTLED'
    | 'REVERSED';

export interface OperationalExpense {
    id: string;
    expenseAccountId: string;
    description: string;
    amount: number;
    settledAmount: number;
    status: ExpenseStatus;
    accountingDate: string;
}

export interface OperationalExpenseSettlement {
    id: string;
    expenseId: string;
    fundingAccountId: string;
    amount: number;
    settlementDate: string;
    reference?: string;
    reversed: boolean;
}

export interface CreateOperationalExpenseRequest {
    branchId: string;
    expenseAccountId: string;
    fundingAccountId?: string;
    description: string;
    amount: number;
    accountingDate: string;
    autoPay: boolean;
    reference?: string;
    sourceModule: string;
    sourceId: string;
}

export interface SettleOperationalExpenseRequest {
    fundingAccountId: string;
    amount: number;
    settlementDate: string;
    reference?: string;
    sourceId?: string;
}

export interface BulkSettleOperationalExpenseRequest {
    expenseIds: string[];
    fundingAccountId: string;
    settlementDate: string;
    reference?: string;
    sourceId: string;
}