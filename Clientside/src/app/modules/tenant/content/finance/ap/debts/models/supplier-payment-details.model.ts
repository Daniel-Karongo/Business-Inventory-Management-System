import { PaymentSettlementDto } from './payment-settlement.model';
import { SupplierPaymentResponseDto } from './supplier-payment.model';

export interface SupplierPaymentDetailsDto {
    payment: SupplierPaymentResponseDto;
    allocations: PaymentSettlementDto[];
}