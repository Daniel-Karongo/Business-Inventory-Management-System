package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatBusinessStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;

public final class VatStatusMapper {

    private VatStatusMapper() {
    }

    public static VatBusinessStatus businessStatus(
            VatFilingStatus status
    ) {

        if (status == null) {
            return VatBusinessStatus.PAID;
        }

        return switch (status) {

            case VAT_PAYABLE ->
                    VatBusinessStatus.PAYMENT_DUE;

            case PARTIALLY_PAID ->
                    VatBusinessStatus.PARTIALLY_PAID;

            case PAID ->
                    VatBusinessStatus.PAID;

            case VAT_CREDIT_CARRIED_FORWARD ->
                    VatBusinessStatus.CREDIT_AVAILABLE;

            case VAT_REFUND_PENDING ->
                    VatBusinessStatus.REFUND_REQUESTED;

            case VAT_REFUNDED ->
                    VatBusinessStatus.REFUNDED;
        };
    }

    public static String displayStatus(
            VatFilingStatus status
    ) {

        if (status == null) {
            return "Completed";
        }

        return switch (status) {

            case VAT_PAYABLE ->
                    "Payment Due";

            case PARTIALLY_PAID ->
                    "Partially Paid";

            case PAID ->
                    "Paid";

            case VAT_CREDIT_CARRIED_FORWARD ->
                    "Credit Available";

            case VAT_REFUND_PENDING ->
                    "Refund Requested";

            case VAT_REFUNDED ->
                    "Refunded";
        };
    }
}