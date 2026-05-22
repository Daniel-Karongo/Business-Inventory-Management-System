package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.config.util.MoneyScale;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.VatBreakdown;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

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

        BigDecimal net;
        BigDecimal vat;
        BigDecimal gross;

        if (vatInclusive) {

            gross = amount;

            net = gross.divide(
                    BigDecimal.ONE.add(vatRate),
                    MoneyScale.INTERNAL_SCALE,
                    MoneyScale.ROUNDING
            );

            vat = gross.subtract(net);

        } else {

            net = amount;

            vat = net.multiply(vatRate);

            gross = net.add(vat);
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