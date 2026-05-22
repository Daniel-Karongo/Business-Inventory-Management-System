export interface SupplierTimelineEntry {
    timestamp: string;
    activity: string;
    activityType?: string;
    reference: string;
    debitAmount: number;
    creditAmount: number;
    runningBalance: number;
    reversed?: boolean;
    reversalReason?: string;
    metadata?: Record<string, any>;
}