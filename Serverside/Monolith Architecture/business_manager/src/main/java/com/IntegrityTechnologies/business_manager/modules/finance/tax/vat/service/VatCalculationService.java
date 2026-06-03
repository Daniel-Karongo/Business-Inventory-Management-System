package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.config.util.MoneyScale;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatBreakdown;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;

@Service
public class VatCalculationService {

    public VatBreakdown calculate(
            BigDecimal amount,
            boolean vatInclusive,
            BigDecimal vatRate
    ) {

        if (amount == null) {
            throw new IllegalArgumentException(
                    "amount is required"
            );
        }

        if (vatRate == null) {
            vatRate = BigDecimal.ZERO;
        }

        if (vatRate.compareTo(BigDecimal.valueOf(100)) > 0) {
            throw new IllegalArgumentException(
                    "VAT rate cannot exceed 100%"
            );
        }

        /*
         * UI/API sends:
         * 16 = 16%
         *
         * Convert to:
         * 0.16
         */
        BigDecimal normalizedRate =
                vatRate.divide(
                        BigDecimal.valueOf(100),
                        MoneyScale.INTERNAL_SCALE,
                        MoneyScale.ROUNDING
                );

        BigDecimal net;
        BigDecimal vat;
        BigDecimal gross;

        if (vatInclusive) {

            gross = amount;

            net =
                    gross.divide(
                            BigDecimal.ONE.add(
                                    normalizedRate
                            ),
                            MoneyScale.INTERNAL_SCALE,
                            MoneyScale.ROUNDING
                    );

            vat =
                    gross.subtract(net);

        } else {

            net = amount;

            vat =
                    net.multiply(
                            normalizedRate
                    );

            gross =
                    net.add(vat);
        }

        return new VatBreakdown(
                net.setScale(
                        MoneyScale.MONEY_SCALE,
                        MoneyScale.ROUNDING
                ),
                vat.setScale(
                        MoneyScale.MONEY_SCALE,
                        MoneyScale.ROUNDING
                ),
                gross.setScale(
                        MoneyScale.MONEY_SCALE,
                        MoneyScale.ROUNDING
                )
        );
    }
}