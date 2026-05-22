export type AccountType =
    | 'ASSET'
    | 'LIABILITY'
    | 'EQUITY'
    | 'INCOME'
    | 'EXPENSE';

export type AccountRole =
    | 'CASH'
    | 'BANK'
    | 'MPESA'
    | 'REVENUE'
    | 'COGS'
    | 'INVENTORY'
    | 'ACCOUNTS_RECEIVABLE'
    | 'ACCOUNTS_PAYABLE'
    | 'VAT_INPUT'
    | 'VAT_OUTPUT'
    | 'VAT_RECEIVABLE'
    | 'VAT_PAYABLE'
    | 'CORPORATE_TAX_PAYABLE'
    | 'CORPORATE_TAX_EXPENSE'
    | 'BRANCH_CLEARING'
    | 'EQUITY'
    | 'PURCHASE_RETURNS'
    | 'PURCHASE_PRICE_VARIANCE'
    | 'GOODS_RECEIVED_NOT_INVOICED';

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