package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Slf4j
@Service
@RequiredArgsConstructor
public class SalesService {

    private final SaleRepository saleRepository;
    private final ProductVariantRepository productVariantRepository;
    private final InventoryService inventoryService;
    private final CustomerService customerService;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final PaymentService paymentService;
    private final CustomerRepository customerRepository;

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

        for (var li : req.getItems()) {

            ProductVariant v = productVariantRepository.findById(li.getProductVariantId())
                    .orElseThrow(() ->
                            new IllegalArgumentException("ProductVariant not found: " + li.getProductVariantId()));

            Product p = v.getProduct();

            BigDecimal unitPrice = Optional.ofNullable(li.getUnitPrice())
                    .orElse(v.getMinimumSellingPrice());

            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            lines.add(SaleLineItem.builder()
                    .productVariantId(v.getId())
                    .productName(p.getName() + " (" +
                            (v.getClassification() != null ? v.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(lineTotal)
                    .build());
        }

        BigDecimal computedTotal = lines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        Sale sale = Sale.builder()
                .id(saleId)
                .createdAt(LocalDateTime.now())
                .createdBy(currentUser)
                .customerId(customerId)
                .totalAmount(computedTotal)
                .totalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO))
                .totalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO))
                .lineItems(lines)
                .payments(new ArrayList<>())
                .status(Sale.SaleStatus.CREATED)
                .build();

        Sale saved = saleRepository.save(sale);

        for (SaleLineItem li : lines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "SALE:" + saleId
            );
        }

        return toDTO(saved);
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
            java.time.LocalDate from, java.time.LocalDate to
    ) {

        Pageable pageable = PageRequest.of(page, size, Sort.by("createdAt").descending());
        Page<Sale> pageRes = saleRepository.findAll(pageable);

        List<SaleDTO> mapped = pageRes.stream()
                .filter(s -> status == null || s.getStatus().name().equalsIgnoreCase(status))
                .filter(s -> customer == null || (s.getCustomerId() != null &&
                        s.getCustomerId().toString().equalsIgnoreCase(customer)))
                .filter(s -> branchId == null || s.getLineItems().stream()
                        .anyMatch(li -> branchId.equals(li.getBranchId())))
                .filter(s -> {
                    if (from == null && to == null) return true;
                    if (s.getCreatedAt() == null) return false;
                    var d = s.getCreatedAt().toLocalDate();
                    if (from != null && d.isBefore(from)) return false;
                    if (to != null && d.isAfter(to)) return false;
                    return true;
                })
                .map(this::toDTO)
                .toList();

        return new PageImpl<>(mapped, pageable, mapped.size());
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

        // release existing reservations
        for (SaleLineItem li : sale.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "UPDATE_RELEASE:" + id
            );
        }

        UUID customerId = customerService.findOrCreateCustomer(
                List.of(req.getCustomerIdentifiers())
        );

        List<SaleLineItem> newLines = new ArrayList<>();

        for (var li : req.getItems()) {

            ProductVariant variant = productVariantRepository.findById(li.getProductVariantId())
                    .orElseThrow(() ->
                            new IllegalArgumentException("Variant not found: " + li.getProductVariantId()));

            BigDecimal unitPrice = Optional.ofNullable(li.getUnitPrice())
                    .orElse(variant.getMinimumSellingPrice());

            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            newLines.add(SaleLineItem.builder()
                    .productVariantId(variant.getId())
                    .productName(variant.getClassification())
                    .branchId(li.getBranchId())
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(lineTotal)
                    .build());
        }

        BigDecimal newTotal = newLines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        sale.getLineItems().clear();
        sale.getLineItems().addAll(newLines);

        sale.setCustomerId(customerId);
        sale.setTotalAmount(newTotal);
        sale.setTotalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO));
        sale.setTotalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO));

        saleRepository.save(sale);

        for (SaleLineItem li : newLines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "SALE_UPDATE:" + id
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
                    li.getQuantity(),
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

    /* ============================================================
       RETURN STOCK TO INVENTORY
       ============================================================ */
        for (SaleLineItem li : sale.getLineItems()) {
            inventoryService.incrementVariantStock(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "REFUND:" + id
            );
        }

    /* ============================================================
       CALCULATE TOTAL PAID (SUCCESS PAYMENTS ONLY)
       ============================================================ */
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(p -> "SUCCESS".equalsIgnoreCase(p.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

    /* ============================================================
       MARK PAYMENTS AS REFUNDED (NO ACCOUNTING HERE)
       ============================================================ */
        sale.getPayments().forEach(p -> {
            if ("SUCCESS".equalsIgnoreCase(p.getStatus())) {
                p.setStatus("REFUNDED");
            }
        });

    /* ============================================================
       RECORD CUSTOMER REFUND (NON-FINANCIAL)
       ============================================================ */
        if (sale.getCustomerId() != null && totalPaid.compareTo(BigDecimal.ZERO) > 0) {
            customerService.recordRefund(
                    sale.getCustomerId(),
                    sale.getId(),
                    totalPaid,
                    "REFUND:" + id
            );
        }

    /* ============================================================
       FINALIZE SALE STATE
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
                        li.getQuantity(),
                        "CANCEL+REFUND_RELEASE:" + id
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
                .createdAt(sale.getCreatedAt())
                .createdBy(sale.getCreatedBy())
                .totalAmount(sale.getTotalAmount())
                .totalTax(sale.getTotalTax())
                .totalDiscount(sale.getTotalDiscount())
                .status(sale.getStatus().name())
                .customerId(sale.getCustomerId())
                .items(
                        sale.getLineItems().stream().map(li ->
                                SaleLineItemDTO.builder()
                                        .productVariantId(li.getProductVariantId())
                                        .productName(li.getProductName())
                                        .branchId(li.getBranchId())
                                        .quantity(li.getQuantity())
                                        .unitPrice(li.getUnitPrice())
                                        .lineTotal(li.getLineTotal())
                                        .build()
                        ).toList()
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
}