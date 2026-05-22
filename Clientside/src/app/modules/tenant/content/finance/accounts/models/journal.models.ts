export type EntryDirection =
    | 'DEBIT'
    | 'CREDIT';

export interface LedgerLine {
    accountId: string;
    accountCode: string;
    accountName: string;
    direction: EntryDirection;
    amount: number;
}

export interface Journal {
    id: string;
    reference: string;
    description: string;
    sourceModule: string;
    postedBy: string;
    postedAt: string;
    entries: LedgerLine[];
}

export interface ManualJournalLine {
    accountId: string;
    direction: EntryDirection;
    amount: number;
}

export interface ManualJournalRequest {
    reference: string;
    description: string;
    branchId: string;
    accountingDate: string;
    lines: ManualJournalLine[];
}

export interface JournalReversalRequest {
    reason: string;
}