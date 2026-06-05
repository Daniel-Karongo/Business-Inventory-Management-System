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
    periodStart: string;
    periodEnd: string;
    taxableProfit: number;
    taxRate: number;
    taxAmount: number;
    paidAmount: number;
    outstandingAmount: number;
    status:
    | 'ACCRUED'
    | 'PARTIALLY_PAID'
    | 'PAID';
    paid: boolean;
    filedAt: string;
    paidAt?: string | null;
}

export interface AccountingPeriod {
    id: string;
    branchId: string;
    startDate: string;
    endDate: string;
    closed: boolean;

    taxAccrued: boolean;
    taxAccruedAt?: string | null;
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

export interface CorporateTaxOverview {
    outstandingTax: number;
    estimatedTax: number;
    taxableProfit: number;
    pendingAccruals: number;
    pendingPayments: number;
    totalPaidTax: number;
    totalAccruedTax: number;
}

export interface CorporateTaxAccrualPreview {
    periodId: string;
    startDate: string;
    endDate: string;
    revenue: number;
    expenses: number;
    taxableProfit: number;
    taxRate: number;
    estimatedTax: number;
}

export type VatBusinessStatus =
    | 'PAYMENT_DUE'
    | 'PARTIALLY_PAID'
    | 'PAID'
    | 'CREDIT_AVAILABLE'
    | 'REFUND_REQUESTED'
    | 'REFUNDED';

export interface VatDashboard {
    currentVatPosition: number;
    outstandingVat: number;
    availableCredit: number;
    pendingRefundAmount: number;
    totalVatPaid: number;

    latestFilingId: string | null;
    latestFiledPeriodStart: string | null;
    latestFiledPeriodEnd: string | null;
    latestFilingStatus: VatBusinessStatus | null;

    nextUnfiledPeriodId: string | null;
    nextUnfiledPeriodStart: string | null;
    nextUnfiledPeriodEnd: string | null;

    filingRequired: boolean;
    vatRate: number;
}

export interface VatOverview {
    vatToPay: number;
    creditAvailable: number;

    refundsPending: number;

    lastFilingId: string | null;
    lastReturnStart: string | null;
    lastReturnEnd: string | null;

    nextReturnPeriodId: string | null;
    nextReturnStart: string | null;
    nextReturnEnd: string | null;
}

export interface VatFilingPreview {
    periodId: string;

    periodStart: string;
    periodEnd: string;

    outputVat: number;
    inputVat: number;

    openingCredit: number;
    creditApplied: number;

    netVatDue: number;
    generatedCredit: number;

    outcome: string;
    recommendedAction: string;

    warnings: string[];
}

export interface VatFilingReadiness {
    ready: boolean;
    periodClosed: boolean;
    alreadyFiled: boolean;

    message: string;
    warnings: string[];
}

export interface VatFilingSummary {
    filingId: string;
    periodId: string;

    periodLabel: string;

    vatDue: number;
    vatCredit: number;

    paidAmount: number;
    outstandingAmount: number;

    displayStatus: string;

    filedAt: string;
}

export interface VatFilingDetail {
    filingId: string;
    periodId: string;

    outputVat: number;
    inputVat: number;

    openingCredit: number;
    creditApplied: number;
    closingCredit: number;

    vatPayable: number;
    vatReceivableCreated: number;

    paidAmount: number;
    outstandingAmount: number;

    businessStatus: VatBusinessStatus;

    displayStatus: string;
    summaryMessage: string;

    paid: boolean;

    filedAt: string;
    paidAt?: string | null;
}

export interface VatPayment {
    id: string;
    filingId: string;

    amount: number;

    fundingAccountId: string;

    recordedBy: string;
    recordedAt: string;
}

export interface VatRefund {
    id: string;
    filingId: string;

    amount: number;

    status:
    | 'REQUESTED'
    | 'COMPLETED';

    requestedBy: string;
    requestedAt: string;

    processedBy?: string | null;
    processedAt?: string | null;
}

export interface VatCreditMovement {
    id: string;
    filingId: string;

    type:
    | 'GENERATED'
    | 'CONSUMED'
    | 'REFUNDED';

    amount: number;

    createdAt: string;
}

export interface RecordVatPaymentRequest {
    fundingAccountId: string;
    amount: number;
}