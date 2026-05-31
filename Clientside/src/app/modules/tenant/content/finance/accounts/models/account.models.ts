export type AccountType =
    | 'ASSET'
    | 'LIABILITY'
    | 'EQUITY'
    | 'INCOME'
    | 'EXPENSE';

export type AccountRole = string;

export interface Account {
    id: string;
    code: string;
    name: string;
    type: string;
    role: string;
    active: boolean;
    balance: number;
    updatedAt?: string | null;
}

export interface CreateAccountRequest {
    branchId: string;
    code: string;
    name: string;
    type: string;
    role: string;
}

export interface UpdateAccountRequest {
    name: string;
}