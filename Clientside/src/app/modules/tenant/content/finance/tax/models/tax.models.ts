export interface TaxStatus {
    vatEnabled: boolean;
    vatRate: number;
    corporateTaxRate: number;
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
    locked: boolean;
    lockedAt?: string | null;
    branchId: string;
}

export interface VatFiling {
    id: string;
    outputVat: number;
    inputVat: number;
    vatPayable: number;
    paid: boolean;
    filedAt: string;
    filedBy?: string;
    period?: {
        id: string;
        startDate: string;
        endDate: string;
    };
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

export interface TaxPeriod {
  id: string;
  startDate: string;
  endDate: string;
  closed: boolean;
  closedBy?: string | null;
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