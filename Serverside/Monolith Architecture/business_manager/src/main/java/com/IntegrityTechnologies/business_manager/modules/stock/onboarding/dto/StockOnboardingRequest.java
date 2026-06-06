package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
public class StockOnboardingRequest {

    /* =====================================================
       PRODUCT
    ===================================================== */

    private String productName;
    private UUID productId;
    private Long categoryId;
    private String newCategoryName;
    private Boolean createCategoryIfMissing;
    private Boolean createProductIfMissing;
    private List<UUID> supplierIds;
    private Double minimumPercentageProfit;

    /* =====================================================
       VARIANT
    ===================================================== */

    private String classification;
    private UUID variantId;

    /* =====================================================
       PACKAGING
    ===================================================== */

    private List<PackagingInput> packagings;

    /* =====================================================
       PRICING
    ===================================================== */

    private List<PricingInput> pricing;

    /* =====================================================
       INVENTORY
    ===================================================== */

    private List<SupplierInput> suppliers;
    private UUID branchId;
    private String reference;
    private String note;
    private LocalDate accountingDate;

    /* =====================================================
       OPTIONAL OPERATIONAL EXPENSES
    ===================================================== */

    private List<OperationalExpenseInput> operationalExpenses;

    /* =====================================================
       AUTO PAYMENT
    ===================================================== */

    private Boolean autoPaySuppliers;
    private SupplierPaymentMethod supplierPaymentMethod;
    private Boolean autoPayOperationalExpenses;
    private UUID fundingAccountId;

    /* =====================================================
       PACKAGING INPUT
    ===================================================== */

    @Data
    public static class PackagingInput {
        private String name;
        private Long units;
    }

    /* =====================================================
       PRICING INPUT
    ===================================================== */

    @Data
    public static class PricingInput {
        private String packagingName;
        private BigDecimal sellingPrice;
    }

    /* =====================================================
       SUPPLIER INPUT
    ===================================================== */

    @Data
    public static class SupplierInput {
        private UUID supplierId;
        private String supplierName;
        private Boolean createSupplierIfMissing;
        private Long unitsSupplied;
        private BigDecimal unitCost;
        private String packagingName;
        private Boolean vatInclusive;
        private BigDecimal vatRate;
    }

    /* =====================================================
   OPERATIONAL EXPENSE INPUT
===================================================== */

    @Data
    public static class OperationalExpenseInput {

        private UUID expenseAccountId;

        private String description;

        private BigDecimal amount;
    }
}