package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.sales.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.security.core.context.SecurityContextHolder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SalesService {

    private final SaleRepository saleRepository;
    private final PaymentRepository paymentRepository;
    private final ProductRepository productRepository;
    private final InventoryService inventoryService; // must exist or adapt

    @Transactional
    public SaleResponse createSale(SaleRequest req) {
        UUID saleId = UUID.randomUUID();
        String currentUser = getCurrentUsername();

        // build line items (resolve product names/prices if missing)
        List<SaleLineItem> lines = new ArrayList<>();
        for (var li : req.getItems()) {
            Product p = productRepository.findById(li.getProductId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found: " + li.getProductId()));
            BigDecimal unitPrice = li.getUnitPrice() != null ? li.getUnitPrice() : p.getPrice();
            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(li.getQuantity()));
            SaleLineItem line = SaleLineItem.builder()
                    .productId(li.getProductId())
                    .productName(p.getName())
                    .unitPrice(unitPrice)
                    .quantity(li.getQuantity())
                    .lineTotal(lineTotal)
                    .build();
            lines.add(line);
        }

        // total validation/compute
        BigDecimal computedTotal = lines.stream()
                .map(SaleLineItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // create payments
        List<Payment> payments = Optional.ofNullable(req.getPayments()).orElse(Collections.emptyList())
                .stream()
                .map(pd -> {
                    Payment r = Payment.builder()
                            .id(UUID.randomUUID())
                            .timestamp(LocalDateTime.now())
                            .amount(pd.getAmount())
                            .method(pd.getMethod())
                            .providerReference(pd.getReference())
                            .status("COMPLETED") // adapter can update this if external
                            .build();
                    paymentRepository.save(r);
                    return r;
                }).collect(Collectors.toList());

        // create sale record
        Sale sale = Sale.builder()
                .id(saleId)
                .createdAt(LocalDateTime.now())
                .createdBy(currentUser)
                .totalAmount(computedTotal)
                .totalDiscount(Optional.ofNullable(req.getTotalDiscount()).orElse(BigDecimal.ZERO))
                .totalTax(Optional.ofNullable(req.getTotalTax()).orElse(BigDecimal.ZERO))
                .lineItems(lines)
                .payments(payments)
                .status(Sale.SaleStatus.COMPLETED)
                .build();

        // persist sale
        Sale saved = saleRepository.save(sale);

        // decrement inventory for each line (this is where we integrate inventory)
        for (SaleLineItem li : lines) {
            // InventoryService must provide this method; will throw if insufficient stock
            inventoryService.decrementStock(li.getProductId(), li.getQuantity(), "SALE:" + saleId.toString());
        }

        return SaleResponse.builder()
                .saleId(saved.getId())
                .createdAt(saved.getCreatedAt())
                .createdBy(saved.getCreatedBy())
                .status(saved.getStatus().name())
                .build();
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }
}