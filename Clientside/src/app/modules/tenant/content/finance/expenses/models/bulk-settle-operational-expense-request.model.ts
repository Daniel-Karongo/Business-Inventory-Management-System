export interface BulkSettleOperationalExpenseRequest {
    expenseIds: string[];
    fundingAccountId: string;
    settlementDate: string;
    reference?: string;
    sourceId?: string;
}