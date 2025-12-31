package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentServiceImpl;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleCustomerDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleLineItemDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class SalesService {

    private final SaleRepository saleRepository;
    private final ProductRepository productRepository;
    private final ProductVariantRepository productVariantRepository;
    private final InventoryService inventoryService;
    private final CustomerService customerService;
    private final AccountingService accountingService;
    private final PaymentServiceImpl paymentService;
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

        // Reserve stock
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
                .map(s -> toDTO(s))
                .toList();

        return new PageImpl<>(mapped, pageable, mapped.size());
    }

    /* ============================================================
       UPDATE SALE (ONLY IN CREATED STATE)
       ============================================================ */
    @Transactional
    public SaleDTO updateSale(UUID id, SaleRequest req) {

        Sale s = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (s.getStatus() != Sale.SaleStatus.CREATED) {
            throw new IllegalStateException("Only CREATED sales can be updated");
        }

        UUID customerId = customerService.findOrCreateCustomer(List.of(req.getCustomerIdentifiers()));

        // Release existing reservations
        for (SaleLineItem li : s.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "UPDATE_RELEASE:" + id
            );
        }

        // Recreate sale lines
        List<SaleLineItem> newLines = new ArrayList<>();

        for (var li : req.getItems()) {

            ProductVariant variant = productVariantRepository.findById(li.getProductVariantId())
                    .orElseThrow(() ->
                            new IllegalArgumentException("Variant not found: " + li.getProductVariantId()));

            BigDecimal unitPrice = li.getUnitPrice() != null
                    ? li.getUnitPrice()
                    : variant.getMinimumSellingPrice();

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

        // Recalculate totals
        BigDecimal computedTotal = newLines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // Replace sale lines
        s.getLineItems().clear();
        s.getLineItems().addAll(newLines);

        s.setCustomerId(customerId);
        s.setTotalAmount(computedTotal);
        s.setTotalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO));
        s.setTotalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO));

        saleRepository.save(s);

        // Re-reserve stock
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
       CANCEL SALE (WITHOUT REFUND)
       ============================================================ */
    @Transactional
    public SaleDTO cancelSale(UUID id) {

        Sale s = saleRepository.findById(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Sale not found: " + id));

        if (s.getStatus() == Sale.SaleStatus.CANCELLED) return getSale(id);
        if (s.getStatus() == Sale.SaleStatus.REFUNDED) {
            throw new IllegalStateException("Cannot cancel a REFUNDED sale");
        }

        for (SaleLineItem li : s.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "CANCEL:" + id
            );
        }

        s.setStatus(Sale.SaleStatus.CANCELLED);
        saleRepository.save(s);

        return getSale(id);
    }

    /* ============================================================
       FULL REFUND WORKFLOW
       ============================================================ */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public SaleDTO refundSale(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        log.info("🔄 REFUND SALE {}", id);

        if (sale.getStatus() == Sale.SaleStatus.REFUNDED) return toDTO(sale);

        // 1️⃣ Restore stock
        for (SaleLineItem li : sale.getLineItems()) {
            inventoryService.incrementVariantStock(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "REFUND:" + id
            );
        }

        // 2️⃣ Reverse accounting
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(p -> "SUCCESS".equalsIgnoreCase(p.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (totalPaid.compareTo(BigDecimal.ZERO) > 0) {
            OptimisticRetryRunner.runWithRetry(() -> {
                accountingService.reverseSalePayment(
                        sale.getId(),
                        totalPaid,
                        "REFUND:" + sale.getId()
                );
                return null;
            });
        }

        // 3️⃣ Update payment statuses
        sale.getPayments().forEach(payment -> {
            if ("SUCCESS".equalsIgnoreCase(payment.getStatus())) {
                payment.setStatus("REFUNDED");
            }
        });

        // 4️⃣ Update customer history
        if (sale.getCustomerId() != null) {
            customerService.recordRefund(
                    sale.getCustomerId(),
                    sale.getId(),
                    totalPaid,
                    "REFUND:" + id
            );
        }

        // 5️⃣ Mark sale as REFUNDED
        sale.setStatus(Sale.SaleStatus.REFUNDED);
        saleRepository.save(sale);

        return toDTO(sale);
    }

    /* ============================================================
       CANCEL + REFUND (ONE ACTION)
       ============================================================ */
    @Transactional
    public SaleDTO cancelAndRefundSale(UUID id) {

        Sale sale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        log.info("🚨 CANCEL + REFUND SALE {}", id);

        if (sale.getStatus() == Sale.SaleStatus.REFUNDED) return toDTO(sale);

        // 1️⃣ Release outstanding reservations (if any)
        for (SaleLineItem li : sale.getLineItems()) {
            try {
                inventoryService.releaseReservationVariant(
                        li.getProductVariantId(),
                        li.getBranchId(),
                        li.getQuantity(),
                        "CANCEL+REFUND_RELEASE:" + id
                );
            } catch (Exception ignored) {
            }
        }

        // 2️⃣ Refund the sale (restores stock, reverses payments)
        // call refundSale but get entity again for updating
        refundSale(id); // returns DTO but performs refund internally

        Sale refundedSale = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found after refund: " + id));

        // mark cancelled AFTER refund
        refundedSale.setStatus(Sale.SaleStatus.CANCELLED);
        saleRepository.save(refundedSale);

        return toDTO(sale);
    }

    /* ============================================================
       HELPERS
       ============================================================ */
//    private String getCurrentUsername() {
//        var auth = SecurityContextHolder.getContext().getAuthentication();
//        return auth != null ? auth.getName() : "SYSTEM";
//    }

    @Transactional(readOnly = true)
    public Iterable<com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO> getSalePayments(UUID id) {
        Sale s = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        return s.getPayments().stream()
                .map(p -> paymentService.toDTO(p))
                .toList();
    }

    @Transactional(readOnly = true)
    public SaleDTO getReceipt(UUID id) {
        return getSale(id);
    }

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