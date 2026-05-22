export interface LedgerRow {
    journalId: string;
    journalReference: string;
    postedAt: string;
    direction: 'DEBIT' | 'CREDIT';
    amount: number;
    runningBalance: number;
}