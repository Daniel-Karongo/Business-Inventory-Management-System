package com.IntegrityTechnologies.business_manager.modules.finance.tax.config;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Data
@Component
@ConfigurationProperties(prefix = "tax")
public class TaxProperties {

    private boolean vatEnabled = true;
    private BigDecimal vatRate = new BigDecimal("0.16");
    private boolean pricesVatInclusive = true;
    private BigDecimal corporateTaxRate = new BigDecimal("0.30");

    // 🔥 NEW
    private String taxMode = "CORPORATE";

    public BusinessTaxMode getBusinessTaxMode() {
        return BusinessTaxMode.valueOf(taxMode.toUpperCase());
    }
}