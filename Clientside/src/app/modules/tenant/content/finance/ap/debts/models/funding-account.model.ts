export interface FundingAccount {
    id: string;
    code: string;
    name: string;
    role: 'CASH' | 'BANK' | 'MPESA';
    active: boolean;
    balance: number;
}