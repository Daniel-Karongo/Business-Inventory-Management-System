export interface TaxStatus {

    vatEnabled: boolean;

    vatRate: number;

    corporateTaxRate: number;

    vatCreditTreatment:
    | 'CARRY_FORWARD'
    | 'REFUND'
    | 'BOTH';

    lastAccrualDate: string | null;

    locked: boolean;
}

export interface TaxSystemState {
    id?: string;

    taxMode:
    | 'CORPORATE'
    | 'PERSONAL'
    | 'NONE';

    vatEnabled: boolean;

    pricesVatInclusive: boolean;

    vatRate: number;

    corporateTaxRate: number;

    vatCreditTreatment:
    | 'CARRY_FORWARD'
    | 'REFUND'
    | 'BOTH';

    locked: boolean;

    lockedAt?: string | null;

    branchId: string;
}

export interface VatFiling {
    id: string;
    periodId: string;

    outputVat: number;
    inputVat: number;

    vatPayable: number;

    openingCredit: number;
    creditApplied: number;
    closingCredit: number;

    vatReceivableCreated: number;

    status:
    | 'VAT_PAYABLE'
    | 'PAID'
    | 'VAT_CREDIT_AVAILABLE'
    | 'VAT_CREDIT_CARRIED_FORWARD'
    | 'VAT_REFUND_PENDING'
    | 'VAT_REFUNDED';

    paid: boolean;

    filedAt: string;

    paidAt?: string | null;
}

export interface CorporateTaxFiling {
    id: string;
    periodId: string;
    taxableProfit: number;
    taxRate: number;
    taxAmount: number;
    paid: boolean;
    filedBy?: string;
    filedAt: string;
    paidAt?: string | null;
}

export interface AccountingPeriod {
    id: string;
    branchId: string;
    startDate: string;
    endDate: string;
    closed: boolean;
}

export interface PageResponse<T> {
    content: T[];
    totalElements: number;
    totalPages: number;
    number: number;
    size: number;
    first: boolean;
    last: boolean;
}