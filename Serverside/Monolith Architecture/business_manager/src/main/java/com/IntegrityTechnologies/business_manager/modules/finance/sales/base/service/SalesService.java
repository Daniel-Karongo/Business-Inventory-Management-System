package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.RevenueRecognitionService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.PaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.ResolutionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableContext;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableResolutionService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.BatchConsumptionDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine.StockEngine;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingPolicy;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
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

    private final ObjectMapper objectMapper;
    private final SellableResolutionService sellableResolutionService;
    private final StockEngine stockEngine;

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

        UUID customerGroupId = resolveCustomerGroupId(customerId);

        List<SaleLineItem> lines = new ArrayList<>();

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

            // =========================
            // BATCH IDS (STRICT)
            // =========================
            List<UUID> batchIds = null;

            if (li.getBatchSelections() != null && !li.getBatchSelections().isEmpty()) {

                batchIds = li.getBatchSelections().stream()
                        .map(BatchSelectionDto::getBatchId)
                        .toList();
            }

            // =========================
            // PRICING POLICY (manual override support)
            // =========================
            PricingPolicy policy = PricingPolicy.builder()
                    .enforceMinimumPrice(!req.isOverrideMinimumPrice())
                    .build();

            // =========================
            // RESOLVE VIA DOMAIN
            // =========================

            SellableContext ctx = SellableContext.builder()
                    .tenantId(tenantId())
                    .branchId(li.getBranchId())
                    .productVariantId(li.getProductVariantId())
                    .packagingId(li.getPackagingId())
                    .quantity(li.getQuantity())
                    .customerId(customerId)
                    .customerGroupId(customerGroupId)
                    .batchIds(batchIds)
                    .pricingPolicy(policy)
                    .mode(ResolutionMode.FINAL_STRICT)
                    .build();

            SellableSnapshot snap = sellableResolutionService.resolve(ctx);

            // =========================
            // STRICT BATCH VALIDATION (UNCHANGED)
            // =========================
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

                if (totalSelected != snap.getBaseUnits()) {
                    throw new IllegalArgumentException(
                            "Batch selections must equal base units. Expected=" +
                                    snap.getBaseUnits() + ", got=" + totalSelected
                    );
                }
            }

            // =========================
            // BUILD LINE (FROM SNAPSHOT)
            // =========================
            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(snap.getProductVariantId())
                    .productName(product.getName() + " (" +
                            (variant.getClassification() != null ? variant.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())

                    .resolutionMode(ctx.getMode().name())

                    // PACKAGING
                    .packagingId(snap.getPackagingId())
                    .unitsPerPackaging(
                            snap.getQuantity() == 0 ? 0 :
                                    snap.getBaseUnits() / snap.getQuantity()
                    )

                    // QUANTITIES
                    .quantity(snap.getQuantity())
                    .baseUnits(snap.getBaseUnits())

                    // PRICE
                    .unitPrice(snap.getUnitPrice())
                    .lineTotal(snap.getTotalPrice())

                    // TAX
                    .netAmount(snap.getNetAmount())
                    .vatAmount(snap.getVatAmount())
                    .vatRate(snap.getVatRate())

                    // 🔥 NEW SNAPSHOT FIELDS
                    .unitCost(snap.getUnitCost())
                    .totalCost(snap.getTotalCost())
                    .availableStockAtSale(snap.getAvailableStock())
                    .stockSufficient(snap.getStockSufficient())
                    .warningsJson(toJsonSafe(snap.getWarnings()))

                    // AUDIT
                    .pricingBreakdownJson(snap.getPricingJson())

                    .batchSelections(new ArrayList<>())
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

        // =========================
        // RESERVE STOCK
        // =========================
        for (SaleLineItem li : lines) {
            stockEngine.reserveWithSelection(
                    saleId,
                    li.getProductVariantId(),
                    li.getPackagingId(),      // ✅ CRITICAL
                    li.getBranchId(),
                    li.getBaseUnits(),        // base units
                    li.getQuantity(),         // sell units
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

            stockEngine.consume(
                    li.getBranchId(),
                    sale.getId()
            );

            totalCOGS = batchConsumptionRepository.sumCostBySaleId(saleId, tenantId());
        }

        sale.setCostOfGoodsSold(totalCOGS);
        saleRepository.save(sale);

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

        // RELEASE OLD
        for (SaleLineItem li : sale.getLineItems()) {
            stockEngine.release(
                    li.getBranchId(),
                    sale.getId()
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

            SellableContext ctx = SellableContext.builder()
                    .tenantId(tenantId())
                    .branchId(li.getBranchId())
                    .productVariantId(li.getProductVariantId())
                    .packagingId(li.getPackagingId())
                    .quantity(li.getQuantity())
                    .customerId(customerId)
                    .customerGroupId(customerGroupId)
                    .batchIds(null)
                    .pricingPolicy(PricingPolicy.builder()
                            .enforceMinimumPrice(true)
                            .build())
                    .mode(ResolutionMode.FINAL_STRICT)
                    .build();

            SellableSnapshot snap = sellableResolutionService.resolve(ctx);

            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(snap.getProductVariantId())
                    .productName(product.getName() + " (" +
                            (variant.getClassification() != null ? variant.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())

                    .resolutionMode(ctx.getMode().name())

                    // PACKAGING
                    .packagingId(snap.getPackagingId())
                    .unitsPerPackaging(
                            snap.getQuantity() == 0 ? 0 :
                                    snap.getBaseUnits() / snap.getQuantity()
                    )

                    // QUANTITIES
                    .quantity(snap.getQuantity())
                    .baseUnits(snap.getBaseUnits())

                    // PRICE
                    .unitPrice(snap.getUnitPrice())
                    .lineTotal(snap.getTotalPrice())

                    // TAX
                    .netAmount(snap.getNetAmount())
                    .vatAmount(snap.getVatAmount())
                    .vatRate(snap.getVatRate())

                    // 🔥 NEW SNAPSHOT FIELDS
                    .unitCost(snap.getUnitCost())
                    .totalCost(snap.getTotalCost())
                    .availableStockAtSale(snap.getAvailableStock())
                    .stockSufficient(snap.getStockSufficient())
                    .warningsJson(toJsonSafe(snap.getWarnings()))

                    // AUDIT
                    .pricingBreakdownJson(snap.getPricingJson())

                    .batchSelections(new ArrayList<>())
                    .build();

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

        // RE-RESERVE
        for (SaleLineItem li : newLines) {
            stockEngine.reserveWithSelection(
                    sale.getId(),
                    li.getProductVariantId(),
                    li.getPackagingId(),
                    li.getBranchId(),
                    li.getBaseUnits(),
                    li.getQuantity(),
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
            stockEngine.release(
                    li.getBranchId(),
                    sale.getId()
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
                stockEngine.release(
                        li.getBranchId(),
                        sale.getId()
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

    private String toJsonSafe(Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            return "[]";
        }
    }
}