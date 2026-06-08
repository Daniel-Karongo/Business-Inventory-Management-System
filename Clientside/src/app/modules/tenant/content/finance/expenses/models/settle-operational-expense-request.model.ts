export interface SettleOperationalExpenseRequest {
    fundingAccountId: string;
    amount: number;
    settlementDate: string;
    reference?: string;
    sourceId?: string;
}