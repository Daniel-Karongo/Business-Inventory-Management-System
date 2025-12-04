package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;

@Service
@RequiredArgsConstructor
public class SalesService {

    private final SaleRepository saleRepository;
    private final ProductRepository productRepository;
    private final InventoryService inventoryService;
    private final CustomerService customerService;
    private final ProductVariantRepository productVariantRepository;

    @Transactional
    public SaleResponse createSale(SaleRequest req) {

        UUID saleId = UUID.randomUUID();
        String currentUser = getCurrentUsername();

        UUID customerId = customerService.findOrCreateCustomer(List.of(req.getCustomerIdentifiers()));

        List<SaleLineItem> lines = new ArrayList<>();
        for (var li : req.getItems()) {
            // li must now contain productVariantId
            ProductVariant v = productVariantRepository.findById(li.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("ProductVariant not found: " + li.getProductVariantId()));

            Product p = v.getProduct();
            BigDecimal unitPrice = Optional.ofNullable(li.getUnitPrice())
                    .orElseGet(() -> Optional.ofNullable(v.getMinimumSellingPrice())
                            .orElse(BigDecimal.ZERO));
            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(v.getId()) // store variant id
                    .productName(p.getName() + " (" + (v.getClassification() != null ? v.getClassification() : "UNCLASSIFIED") + ")")
                    .branchId(li.getBranchId())
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(lineTotal)
                    .build();
            lines.add(line);
        }

        BigDecimal computedTotal = lines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        Sale sale = Sale.builder()
                .id(saleId)
                .createdAt(LocalDateTime.now())
                .createdBy(currentUser)
                .totalAmount(computedTotal)
                .totalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO))
                .totalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO))
                .lineItems(lines)
                .payments(new ArrayList<>())
                .customerId(customerId)
                .status(Sale.SaleStatus.CREATED)
                .build();

        Sale saved = saleRepository.save(sale);

        // Reserve variant-level stock
        for (SaleLineItem li : lines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "SALE:" + saleId);
        }

        return SaleResponse.builder()
                .saleId(saved.getId())
                .createdAt(saved.getCreatedAt())
                .createdBy(saved.getCreatedBy())
                .status(saved.getStatus().name())
                .build();
    }

    @Transactional(readOnly = true)
    public SaleResponse getSale(UUID id) {
        Sale s = saleRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));
        return SaleResponse.builder()
                .saleId(s.getId())
                .createdAt(s.getCreatedAt())
                .createdBy(s.getCreatedBy())
                .status(s.getStatus() != null ? s.getStatus().name() : null)
                .build();
    }

    @Transactional(readOnly = true)
    public Page<SaleResponse> listSales(int page, int size, String status, String customer, UUID branchId, java.time.LocalDate from, java.time.LocalDate to) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        Page<Sale> pageRes = saleRepository.findAll(pageable);

        List<SaleResponse> mapped = pageRes.stream()
                .filter(s -> status == null || (s.getStatus() != null && s.getStatus().name().equalsIgnoreCase(status)))
                .filter(s -> customer == null || (s.getCustomerId() != null && s.getCustomerId().toString().equalsIgnoreCase(customer)))
                .filter(s -> branchId == null || s.getLineItems().stream().anyMatch(li -> branchId.equals(li.getBranchId())))
                .filter(s -> {
                    if (from == null && to == null) return true;
                    if (s.getCreatedAt() == null) return false;
                    java.time.LocalDate d = s.getCreatedAt().toLocalDate();
                    if (from != null && d.isBefore(from)) return false;
                    if (to != null && d.isAfter(to)) return false;
                    return true;
                })
                .map(s -> SaleResponse.builder()
                        .saleId(s.getId())
                        .createdAt(s.getCreatedAt())
                        .createdBy(s.getCreatedBy())
                        .status(s.getStatus() != null ? s.getStatus().name() : null)
                        .build())
                .toList();

        return new PageImpl<>(mapped, pageable, mapped.size());
    }

    @Transactional
    public SaleResponse cancelSale(UUID id) {
        Sale s = saleRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));
        if (s.getStatus() == Sale.SaleStatus.CANCELLED) return getSale(id);

        // release reserved variant stock
        for (SaleLineItem li : s.getLineItems()) {
            inventoryService.releaseReservationVariant(li.getProductVariantId(), li.getBranchId(), li.getQuantity(), "CANCEL:" + id);
        }

        s.setStatus(Sale.SaleStatus.CANCELLED);
        saleRepository.save(s);
        return getSale(id);
    }

    @Transactional
    public SaleResponse updateSale(UUID id, SaleRequest req) {
        Sale s = saleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));

        if (s.getStatus() != Sale.SaleStatus.CREATED) {
            throw new IllegalStateException("Only CREATED sales can be updated");
        }

        // 1️⃣ RELEASE existing reservations
        for (SaleLineItem li : s.getLineItems()) {
            inventoryService.releaseReservationVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "UPDATE_RELEASE:" + id
            );
        }

        // 2️⃣ REBUILD line items (variant-aware)
        List<SaleLineItem> newLines = new ArrayList<>();

        for (var li : req.getItems()) {

            if (li.getProductVariantId() == null) {
                throw new IllegalArgumentException("productVariantId is required for sale update");
            }

            ProductVariant variant = productVariantRepository.findById(li.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("Variant not found: " + li.getProductVariantId()));

            Product product = variant.getProduct();

            BigDecimal unitPrice = li.getUnitPrice() != null
                    ? li.getUnitPrice()
                    : variant.getMinimumSellingPrice();

            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));

            SaleLineItem line = SaleLineItem.builder()
                    .productVariantId(variant.getId())
                    .productName(variant.getClassification() != null ? variant.getClassification() : "UNCLASSIFIED")
                    .branchId(li.getBranchId())
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(lineTotal)
                    .build();

            newLines.add(line);
        }

        // 3️⃣ UPDATE sale totals
        BigDecimal computedTotal = newLines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        s.setLineItems(newLines);
        s.setTotalAmount(computedTotal);
        s.setTotalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO));
        s.setTotalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO));

        saleRepository.save(s);

        // 4️⃣ RESERVE stock for new lines
        for (SaleLineItem li : newLines) {
            inventoryService.reserveStockVariant(
                    li.getProductVariantId(),
                    li.getBranchId(),
                    li.getQuantity(),
                    "SALE_UPDATE:" + id
            );
        }

        // 5️⃣ return new sale representation
        return getSale(id);
    }

    @Transactional(readOnly = true)
    public Iterable<com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse> getSalePayments(UUID id) {
        Sale s = saleRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Sale not found: " + id));
        return s.getPayments().stream()
                .map(p -> com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse.builder()
                        .paymentId(p.getId())
                        .saleId(s.getId())
                        .amount(p.getAmount())
                        .method(p.getMethod())
                        .status(p.getStatus())
                        .providerReference(p.getProviderReference())
                        .timestamp(p.getTimestamp())
                        .note(p.getNote())
                        .build())
                .toList();
    }

    @Transactional(readOnly = true)
    public SaleResponse getReceipt(UUID id) {
        // for now return receipt info same as getSale (reporting layer will render)
        return getSale(id);
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }
}