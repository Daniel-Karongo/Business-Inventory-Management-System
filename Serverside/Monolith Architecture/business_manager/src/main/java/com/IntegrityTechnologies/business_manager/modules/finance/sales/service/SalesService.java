package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.ProductPriceService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.RevenueRecognitionService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.PaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.BatchConsumptionDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class SalesService {

    private final SaleRepository saleRepository;
    private final ProductVariantRepository productVariantRepository;
    private final InventoryService inventoryService;
    private final CustomerService customerService;
    private final AccountingFacade accountingFacade;
    private final PaymentService paymentService;
    private final CustomerRepository customerRepository;
    private final ReceiptNumberService receiptNumberService;
    private final TaxSystemStateService taxSystemStateService;
    private final JournalEntryRepository journalEntryRepository;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final RevenueRecognitionService revenueRecognitionService;
    private final ProductVariantPackagingService packagingService;
    private final PricingEngineService pricingEngine;
    private final ObjectMapper objectMapper;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ============================================================
       CREATE SALE
       ============================================================ */
    @Transactional
    public SaleDTO createSale(SaleRequest req) {

        UUID saleId = UUID.randomUUID();
        String currentUser = SecurityUtils.currentUsername();

        UUID customerId = Optional.ofNullable(req.getCustomerIdentifiers())
                .map(ci -> customerService.findOrCreateCustomer(List.of(ci)))
                .orElse(null);

        List<SaleLineItem> lines = new ArrayList<>();
        UUID customerGroupId = resolveCustomerGroupId(customerId);

        for (var li : req.getItems()) {

            ProductVariant variant = productVariantRepository.findByIdSafe(
                            li.getProductVariantId(),
                            false,
                            tenantId(),
                            li.getBranchId()
                    )
                    .orElseThrow(() ->
                            new IllegalArgumentException("ProductVariant not found: " + li.getProductVariantId()));

            Product product = variant.getProduct();

            // =====================================================
            // 1. PACKAGING
            // =====================================================
            ProductVariantPackaging packaging;

            if (li.getPackagingId() != null) {
                packaging = packagingService.getPackagings(variant.getId())
                        .stream()
                        .filter(pkg -> pkg.getId().equals(li.getPackagingId()))
                        .findFirst()
                        .orElseThrow(() -> new IllegalArgumentException("Invalid packaging for variant"));
            } else {
                packaging = packagingService.getBasePackaging(variant.getId());
            }

            if (packaging.getUnitsPerPackaging() <= 0) {
                throw new IllegalStateException("Invalid packaging configuration");
            }

            // =====================================================
            // 2. PRICE
            // =====================================================
            PricingResult pricing;

            if (li.getUnitPrice() != null) {

                if (!req.isOverrideMinimumPrice()) {
                    throw new IllegalArgumentException(
                            "Manual price requires override flag"
                    );
                }

                if (req.getOverrideReason() == null || req.getOverrideReason().isBlank()) {
                    throw new IllegalArgumentException(
                            "Override reason is required for manual pricing"
                    );
                }

                // 👤 MANUAL OVERRIDE
                pricing = new PricingResult();
                pricing.setBasePrice(li.getUnitPrice());
                pricing.setFinalPrice(li.getUnitPrice());
                pricing.setResolvedPriceId(null);

                pricing.getAdjustments().add(
                        new PricingAdjustment(
                                "MANUAL_OVERRIDE",
                                "USER_INPUT",
                                BigDecimal.ZERO,
                                "Manual price override"
                        )
                );

                if (variant.getMinimumSellingPrice() != null &&
                        li.getUnitPrice().compareTo(variant.getMinimumSellingPrice()) < 0) {

                    pricing.getAdjustments().add(
                            new PricingAdjustment(
                                    "BELOW_MIN_PRICE",
                                    "SYSTEM_WARNING",
                                    variant.getMinimumSellingPrice().subtract(li.getUnitPrice()),
                                    "Price below minimum selling price"
                            )
                    );
                }

            } else {

                long baseUnits = li.getQuantity() * packaging.getUnitsPerPackaging();

                boolean isOverride = req.isOverrideMinimumPrice();

                if (isOverride && req.getOverrideReason() == null) {
                    throw new IllegalArgumentException("Override reason is required");
                }

                pricing = pricingEngine.resolve(
                        PricingContext.builder()
                                .tenantId(tenantId())
                                .branchId(li.getBranchId())
                                .productVariantId(variant.getId())
                                .packagingId(packaging.getId())
                                .quantity(baseUnits)
                                .customerId(customerId)
                                .customerGroupId(customerGroupId)
                                .pricingTime(LocalDateTime.now())
                                .policy(
                                        PricingPolicy.builder()
                                                .enforceMinimumPrice(!isOverride)
                                                .build()
                                )
                                .build()
                );
            }

            BigDecimal unitPrice = pricing.getFinalPrice();

            // =====================================================
            // 3. BASE UNITS
            // =====================================================
            long baseUnits = li.getQuantity() * packaging.getUnitsPerPackaging();

            // =====================================================
            // 4. STRICT BATCH VALIDATION
            // =====================================================
            List<SaleLineBatchSelection> selections = new ArrayList<>();

            if (li.getBatchSelections() != null && !li.getBatchSelections().isEmpty()) {

                long totalSelected = 0;

                for (BatchSelectionDto bs : li.getBatchSelections()) {

                    long qty = bs.getQuantity();

                    if (qty <= 0) {
                        throw new IllegalArgumentException("Batch selection quantity must be > 0");
                    }

                    totalSelected += qty;

                    selections.add(
                            SaleLineBatchSelection.builder()
                                    .batchId(bs.getBatchId())
                                    .tenantId(tenantId())
                                    .quantity(qty)
                                    .build()
                    );
                }

                if (totalSelected != baseUnits) {
                    throw new IllegalArgumentException(
                            "Batch selections must equal base units. Expected=" + baseUnits + ", got=" + totalSelected
                    );
                }
            }

            // =====================================================
            // 5. TAX (RESTORED FULL LOGIC)
            // =====================================================
            BigDecimal gross = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            var taxState = taxSystemStateService.getOrCreate(li.getBranchId());

            boolean vatEnabled = taxState.isVatEnabled();
            BigDecimal vatRate = vatEnabled ? taxState.getVatRate() : BigDecimal.ZERO;
            boolean pricesVatInclusive = taxState.isPricesVatInclusive();

            BigDecimal net;
            BigDecimal vat;

            if (vatEnabled) {

                if (pricesVatInclusive) {
                    net = gross.divide(
                            BigDecimal.ONE.add(vatRate),
                            6,
                            RoundingMode.HALF_UP
                    );
                    vat = gross.subtract(net);
                } else {
                    net = gross;
                    vat = net.multiply(vatRate);
                    gross = net.add(vat);
                }

            } else {
                net = gross;
                vat = BigDecimal.ZERO;
            }

            // =====================================================
            // 6. BUILD LINE
            // =====================================================
            String pricingJson;
            try {
                pricingJson = objectMapper.writeValueAsString(pricing);
            } catch (Exception e) {
                pricingJson = "{\"error\":\"pricing_serialization_failed\"}";
            }

            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(variant.getId())
                    .productName(product.getName() + " (" +
                            (variant.getClassification() != null ? variant.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())
                    .packagingId(packaging.getId())
                    .unitsPerPackaging(packaging.getUnitsPerPackaging())
                    .baseUnits(baseUnits)
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(gross)
                    .netAmount(net)
                    .vatAmount(vat)
                    .vatRate(vatRate)
                    .batchSelections(new ArrayList<>())
                    .pricingBreakdownJson(pricingJson)
                    .build();

            selections.forEach(sel -> {
                sel.setSaleLineItem(line);
                line.getBatchSelections().add(sel);
            });

            lines.add(line);
        }

        BigDecimal total = lines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        UUID branchId = extractSingleBranch(lines);

        String receiptNo = receiptNumberService.nextSaleReceipt();

        Sale sale = Sale.builder()
                .id(saleId)
                .branchId(branchId)
                .tenantId(tenantId())
                .receiptNo(receiptNo)
                .createdAt(LocalDateTime.now())
                .createdBy(currentUser)
                .customerId(customerId)
                .totalAmount(total)
                .totalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO))
                .totalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO))
                .lineItems(lines)
                .payments(new ArrayList<>())
                .status(Sale.SaleStatus.CREATED)
                .build();

        Sale saved = saleRepository.save(sale);

        // =====================================================
        // 7. RESERVE STOCK
        // =====================================================
        for (SaleLineItem li : lines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getBaseUnits(),
                    "SALE:" + saleId,
                    li.getBatchSelections()
            );
        }

        return toDTO(saved);
    }

    @Transactional
    public SaleDTO deliverSale(UUID saleId) {

        Sale sale = saleRepository.findById(saleId)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + saleId));

        if (sale.getStatus() != Sale.SaleStatus.CREATED) {
            return toDTO(sale);
        }

        // -----------------------------------------
        // 1️⃣ Decrement stock (FIFO + COGS handled inside InventoryService)
        // -----------------------------------------
        BigDecimal totalCOGS = BigDecimal.ZERO;

        for (SaleLineItem li : sale.getLineItems()) {

            inventoryService.decrementVariantStock(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getBaseUnits().intValue(), // 🔥 FIXED
                    "SALE_DELIVERY:" + sale.getId(),
                    li.getBatchSelections()
            );

            List<BatchConsumption> consumptions =
                    batchConsumptionRepository.findBySaleIdAndProductVariantIdAndTenantIdAndBranchId(
                    saleId,
                    li.getProductVariantId(),
                    tenantId(),
                    li.getBranchId()
            );


            for (BatchConsumption bc : consumptions) {
                totalCOGS = totalCOGS.add(
                        bc.getUnitCost()
                                .multiply(BigDecimal.valueOf(bc.getQuantity()))
                );
            }
        }

        sale.setCostOfGoodsSold(totalCOGS);
        saleRepository.save(sale);

        // -----------------------------------------
        // 2️⃣ If DELIVERY mode → post revenue accrual
        // -----------------------------------------
//        if (accountingProperties.getRevenueRecognitionMode()
//                == RevenueRecognitionMode.DELIVERY) {
//
//            if (!accountingFacade.isAlreadyPosted("SALE", sale.getId())) {
//
//                BigDecimal totalNet = sale.getLineItems().stream()
//                        .map(SaleLineItem::getNetAmount)
//                        .reduce(BigDecimal.ZERO, BigDecimal::add);
//
//                BigDecimal totalVat = sale.getLineItems().stream()
//                        .map(SaleLineItem::getVatAmount)
//                        .reduce(BigDecimal.ZERO, BigDecimal::add);
//
//                UUID branchId = extractSingleBranch(sale.getLineItems());
//
//                accountingFacade.post(
//                        AccountingEvent.builder()
//                                .eventId(UUID.randomUUID())
//                                .sourceModule("SALE")
//                                .sourceId(sale.getId())
//                                .reference(sale.getReceiptNo())
//                                .description("Revenue recognized on delivery")
//                                .performedBy(SecurityUtils.currentUsername())
//                                .branchId(branchId)
//                                .entries(List.of(
//                                        AccountingEvent.Entry.builder()
//                                                .accountId(accountingAccounts.accountsReceivable())
//                                                .direction(EntryDirection.DEBIT)
//                                                .amount(totalNet.add(totalVat))
//                                                .build(),
//
//                                        AccountingEvent.Entry.builder()
//                                                .accountId(accountingAccounts.revenue())
//                                                .direction(EntryDirection.CREDIT)
//                                                .amount(totalNet)
//                                                .build(),
//
//                                        AccountingEvent.Entry.builder()
//                                                .accountId(accountingAccounts.outputVat())
//                                                .direction(EntryDirection.CREDIT)
//                                                .amount(totalVat)
//                                                .build()
//                                ))
//                                .build()
//                );
//            }
//        }
        sale.setStatus(Sale.SaleStatus.COMPLETED);
        saleRepository.save(sale);

        revenueRecognitionService.recognizeIfEligible(sale);

        return toDTO(sale);
    }

    /* ============================================================
       GET SALE
       ============================================================ */
    @Transactional(readOnly = true)
    public SaleDTO getSale(UUID id) {
        Sale s = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));
        return toDTO(s);
    }

    /* ============================================================
       LIST SALES
       ============================================================ */
    @Transactional(readOnly = true)
    public Page<SaleDTO> listSales(
            int page, int size,
            String status, String customer,
            UUID branchId,
            LocalDate from, LocalDate to
    ) {

        Pageable pageable =
                PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));

        Sale.SaleStatus saleStatus =
                status != null ? Sale.SaleStatus.valueOf(status.toUpperCase()) : null;

        UUID customerId =
                customer != null ? UUID.fromString(customer) : null;

        LocalDateTime fromDt =
                from != null ? from.atStartOfDay() : null;

        LocalDateTime toDt =
                to != null ? to.atTime(23, 59, 59) : null;

        Page<Sale> pageRes =
                saleRepository.searchSales(
                        saleStatus,
                        customerId,
                        branchId,
                        fromDt,
                        toDt,
                        pageable
                );

        return pageRes.map(this::toDTO);
    }

    /* ============================================================
       UPDATE SALE
       ============================================================ */
    @Transactional
    public SaleDTO updateSale(UUID id, SaleRequest req) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (sale.getStatus() != Sale.SaleStatus.CREATED) {
            throw new IllegalStateException("Only CREATED sales can be updated");
        }

        // =====================================================
        // 1. RELEASE OLD RESERVATIONS
        // =====================================================
        for (SaleLineItem li : sale.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getBaseUnits(),
                    "SALE:" + id
            );
        }

        UUID customerId = Optional.ofNullable(req.getCustomerIdentifiers())
                .map(ci -> customerService.findOrCreateCustomer(List.of(ci)))
                .orElse(null);
        UUID customerGroupId = resolveCustomerGroupId(customerId);

        List<SaleLineItem> newLines = new ArrayList<>();

        for (var li : req.getItems()) {

            ProductVariant variant = productVariantRepository.findByIdSafe(
                            li.getProductVariantId(),
                            false,
                            tenantId(),
                            li.getBranchId()
                    )
                    .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

            Product product = variant.getProduct();

            ProductVariantPackaging packaging;

            if (li.getUnitPrice() != null) {
                throw new IllegalStateException("Manual price override not allowed during update");
            }

            if (li.getPackagingId() != null) {
                packaging = packagingService.getPackagings(variant.getId())
                        .stream()
                        .filter(pkg -> pkg.getId().equals(li.getPackagingId()))
                        .findFirst()
                        .orElseThrow(() -> new IllegalArgumentException("Invalid packaging"));
            } else {
                packaging = packagingService.getBasePackaging(variant.getId());
            }

            long baseUnits = li.getQuantity() * packaging.getUnitsPerPackaging();

            PricingResult pricing = pricingEngine.resolve(
                    PricingContext.builder()
                            .tenantId(tenantId())
                            .branchId(li.getBranchId())
                            .productVariantId(variant.getId())
                            .packagingId(packaging.getId())
                            .quantity(baseUnits)
                            .customerId(customerId)
                            .customerGroupId(customerGroupId)
                            .pricingTime(LocalDateTime.now())
                            .policy(
                                    PricingPolicy.builder()
                                            .enforceMinimumPrice(true) // ALWAYS enforce on update
                                            .build()
                            )
                            .build()
            );

            BigDecimal unitPrice = pricing.getFinalPrice();

            List<SaleLineBatchSelection> selections = new ArrayList<>();

            if (li.getBatchSelections() != null && !li.getBatchSelections().isEmpty()) {

                long totalSelected = 0;

                for (BatchSelectionDto bs : li.getBatchSelections()) {

                    long qty = bs.getQuantity();

                    if (qty <= 0) {
                        throw new IllegalArgumentException("Batch selection quantity must be > 0");
                    }

                    totalSelected += qty;

                    selections.add(
                            SaleLineBatchSelection.builder()
                                    .batchId(bs.getBatchId())
                                    .tenantId(tenantId())
                                    .quantity(qty)
                                    .build()
                    );
                }

                if (totalSelected != baseUnits) {
                    throw new IllegalArgumentException("Batch selections must equal base units");
                }
            }

            BigDecimal gross = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            var taxState = taxSystemStateService.getOrCreate(li.getBranchId());

            boolean vatEnabled = taxState.isVatEnabled();
            BigDecimal vatRate = vatEnabled ? taxState.getVatRate() : BigDecimal.ZERO;
            boolean pricesVatInclusive = taxState.isPricesVatInclusive();

            BigDecimal net;
            BigDecimal vat;

            if (vatEnabled) {

                if (pricesVatInclusive) {
                    net = gross.divide(
                            BigDecimal.ONE.add(vatRate),
                            6,
                            RoundingMode.HALF_UP
                    );
                    vat = gross.subtract(net);
                } else {
                    net = gross;
                    vat = net.multiply(vatRate);
                    gross = net.add(vat);
                }

            } else {
                net = gross;
                vat = BigDecimal.ZERO;
            }

            String pricingJson;
            try {
                pricingJson = objectMapper.writeValueAsString(pricing);
            } catch (Exception e) {
                pricingJson = "{\"error\":\"pricing_serialization_failed\"}";
            }

            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(variant.getId())
                    .productName(product.getName() + " (" +
                            (variant.getClassification() != null ? variant.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())
                    .packagingId(packaging.getId())
                    .unitsPerPackaging(packaging.getUnitsPerPackaging())
                    .baseUnits(baseUnits)
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(gross)
                    .netAmount(net)
                    .vatAmount(vat)
                    .vatRate(vatRate)
                    .batchSelections(new ArrayList<>())
                    .pricingBreakdownJson(pricingJson)
                    .build();

            selections.forEach(sel -> {
                sel.setSaleLineItem(line);
                line.getBatchSelections().add(sel);
            });

            newLines.add(line);
        }

        sale.getLineItems().clear();
        sale.getLineItems().addAll(newLines);

        BigDecimal total = newLines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        sale.setCustomerId(customerId);
        sale.setTotalAmount(total);

        saleRepository.save(sale);

        // =====================================================
        // 2. RE-RESERVE
        // =====================================================
        for (SaleLineItem li : newLines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getBaseUnits(),
                    "SALE:" + id,
                    li.getBatchSelections()
            );
        }

        return getSale(id);
    }

    /* ============================================================
       CANCEL SALE (NO REFUND)
       ============================================================ */
    @Transactional
    public SaleDTO cancelSale(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (sale.getStatus() == Sale.SaleStatus.CANCELLED) return getSale(id);
        if (sale.getStatus() == Sale.SaleStatus.REFUNDED) {
            throw new IllegalStateException("Cannot cancel a REFUNDED sale");
        }

        for (SaleLineItem li : sale.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getBaseUnits(),
                    "CANCEL:" + id
            );
        }

        sale.setStatus(Sale.SaleStatus.CANCELLED);
        saleRepository.save(sale);

        return getSale(id);
    }

    /* ============================================================
       FULL REFUND
       ============================================================ */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public SaleDTO refundSale(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (sale.getStatus() == Sale.SaleStatus.REFUNDED) {
            return toDTO(sale);
        }

        if (sale.getStatus() == Sale.SaleStatus.CANCELLED) {
            throw new IllegalStateException("Cannot refund a CANCELLED sale");
        }

        String user = SecurityUtils.currentUsername();

    /* ============================================================
       1️⃣ REVERSE SALE REVENUE JOURNAL (if exists)
    ============================================================ */

        journalEntryRepository
                .findByTenantIdAndSourceModuleAndSourceId(tenantId(), "SALE", sale.getId())
                .ifPresent(journal ->
                        accountingFacade.reverseJournal(
                                journal.getId(),
                                "Sale refund",
                                user
                        )
                );

    /* ============================================================
       2️⃣ REVERSE INVENTORY CONSUMPTION JOURNALS (FIFO COGS)
    ============================================================ */

        List<JournalEntry> consumptionJournals =
                journalEntryRepository.findByTenantIdAndBranchIdAndReference(
                        tenantId(),
                        extractSingleBranch(sale.getLineItems()),
                        "SALE_DELIVERY:" + sale.getId()
                );

        for (JournalEntry journal : consumptionJournals) {

            if (!journal.isReversed()
                    && "INVENTORY_CONSUMPTION".equals(journal.getSourceModule())) {

                accountingFacade.reverseJournal(
                        journal.getId(),
                        "Inventory return (refund)",
                        user
                );
            }
        }

    /* ============================================================
       3️⃣ REVERSE PAYMENT JOURNALS
    ============================================================ */

        for (Payment payment : sale.getPayments()) {

            if (payment.getStatus() == PaymentStatus.SUCCESS) {

                journalEntryRepository
                        .findByTenantIdAndSourceModuleAndSourceId(tenantId(), "PAYMENT", payment.getId())
                        .ifPresent(journal ->
                                accountingFacade.reverseJournal(
                                        journal.getId(),
                                        "Payment refund",
                                        user
                                )
                        );

                payment.setStatus(PaymentStatus.REFUNDED);
            }
        }

        for (SaleLineItem li : sale.getLineItems()) {

            inventoryService.restoreConsumedBatches(
                    sale.getId(),
                    li.getBranchId(),
                    li.getProductVariantId()
            );
        }
    /* ============================================================
       4️⃣ CUSTOMER REFUND RECORD (non-financial)
    ============================================================ */

        BigDecimal totalPaid = sale.getPayments().stream()
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (sale.getCustomerId() != null
                && totalPaid.compareTo(BigDecimal.ZERO) > 0) {

            customerService.recordRefund(
                    sale.getCustomerId(),
                    sale.getId(),
                    totalPaid,
                    "REFUND:" + id
            );
        }

    /* ============================================================
       5️⃣ FINALIZE STATE
    ============================================================ */

        sale.setStatus(Sale.SaleStatus.REFUNDED);
        saleRepository.save(sale);

        return toDTO(sale);
    }

    /* ============================================================
       CANCEL + REFUND
       ============================================================ */
    @Transactional
    public SaleDTO cancelAndRefundSale(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (sale.getStatus() == Sale.SaleStatus.REFUNDED) return toDTO(sale);

        for (SaleLineItem li : sale.getLineItems()) {
            try {
                inventoryService.releaseReservationVariant(
                        li.getProductVariantId(),
                        li.getBranchId(),
                        li.getBaseUnits(),
                        "RELEASE:" + id
                );
            } catch (Exception ignored) {}
        }

        refundSale(id);

        Sale refundedSale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found after refund: " + id));

        refundedSale.setStatus(Sale.SaleStatus.CANCELLED);
        saleRepository.save(refundedSale);

        return toDTO(refundedSale);
    }

    /* ============================================================
       PAYMENTS FOR SALE (FIXED TYPE ERROR)
       ============================================================ */
    @Transactional(readOnly = true)
    public Iterable<PaymentDTO> getSalePayments(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        return sale.getPayments().stream()
                .map(p -> paymentService.getPayment(p.getId())) // ✅ FIXED
                .toList();
    }

    /* ============================================================
       RECEIPT
       ============================================================ */
    @Transactional(readOnly = true)
    public SaleDTO getReceipt(UUID id) {
        return getSale(id);
    }

    /* ============================================================
       DTO MAPPER
       ============================================================ */
    public SaleDTO toDTO(Sale sale) {

        SaleCustomerDTO customerDto = null;

        if (sale.getCustomerId() != null) {
            Customer c = customerRepository.findById(sale.getCustomerId()).orElse(null);
            if (c != null) {
                customerDto = SaleCustomerDTO.builder()
                        .id(c.getId())
                        .name(c.getName())
                        .phoneNumbers(c.getPhoneNumbers())
                        .emailAddresses(c.getEmailAddresses())
                        .build();
            }
        }

        return SaleDTO.builder()
                .id(sale.getId())
                .receiptNo(sale.getReceiptNo() != null ? sale.getReceiptNo() : sale.getId().toString())
                .createdAt(sale.getCreatedAt())
                .createdBy(sale.getCreatedBy())
                .totalAmount(sale.getTotalAmount())
                .totalTax(sale.getTotalTax())
                .totalDiscount(sale.getTotalDiscount())
                .status(sale.getStatus().name())
                .customerId(sale.getCustomerId())
                .items(
                        sale.getLineItems().stream().map(li -> {

                            List<BatchConsumptionDTO> consumptions =
                                    batchConsumptionRepository.findBySaleIdAndProductVariantIdAndTenantIdAndBranchId(
                                                    sale.getId(),
                                                    li.getProductVariantId(),
                                                    tenantId(),
                                                    li.getBranchId()
                                            )
                                            .stream()
                                            .map(bc -> BatchConsumptionDTO.builder()
                                                    .batchId(bc.getBatch().getId())
                                                    .saleId(bc.getSaleId())
                                                    .productVariantId(bc.getProductVariantId())
                                                    .quantity(bc.getQuantity())
                                                    .unitCost(bc.getUnitCost())
                                                    .totalCost(
                                                            bc.getUnitCost()
                                                                    .multiply(BigDecimal.valueOf(bc.getQuantity()))
                                                    )
                                                    .build()
                                            )
                                            .toList();

                            return SaleLineItemDTO.builder()
                                    .productVariantId(li.getProductVariantId())
                                    .productName(li.getProductName())
                                    .branchId(li.getBranchId())
                                    .quantity(li.getQuantity())
                                    .unitPrice(li.getUnitPrice())
                                    .lineTotal(li.getLineTotal())
                                    .batchConsumptions(consumptions)
                                    .build();
                        }).toList()
                )
                .payments(
                        sale.getPayments().stream().map(p ->
                                PaymentDTO.builder()
                                        .paymentId(p.getId())
                                        .amount(p.getAmount())
                                        .method(p.getMethod())
                                        .status(p.getStatus())
                                        .providerReference(p.getProviderReference())
                                        .timestamp(p.getTimestamp())
                                        .note(p.getNote())
                                        .transactionCode(p.getTransactionCode())
                                        .build()
                        ).toList()
                )
                .customer(customerDto)
                .build();
    }

    private UUID extractSingleBranch(List<SaleLineItem> lines) {

        Set<UUID> branches = lines.stream()
                .map(SaleLineItem::getBranchId)
                .collect(Collectors.toSet());

        if (branches.size() != 1) {
            throw new IllegalStateException(
                    "A sale must belong to a single branch. Multiple branches detected."
            );
        }

        return branches.iterator().next();
    }

    private UUID resolveCustomerGroupId(UUID customerId) {
        if (customerId == null) return null;

        return customerRepository.findById(customerId)
                .map(c -> c.getGroup() != null ? c.getGroup().getId() : null)
                .orElse(null);
    }
}