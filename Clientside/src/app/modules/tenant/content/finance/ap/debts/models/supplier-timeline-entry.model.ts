export interface SupplierTimelineEntry {
    timestamp: string;
    activity: string;
    reference: string;
    debitAmount: number;
    creditAmount: number;
    runningBalance: number;
}