package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service;

import com.IntegrityTechnologies.business_manager.exception.ExpectedConcurrencyException;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoiceLine;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.CreatePurchaseInvoiceLineRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.CreatePurchaseInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.PurchaseInvoiceResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.mapper.PurchaseInvoiceMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.validation.ApValidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.service.DocumentNumberGeneratorService;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PurchaseInvoiceService {

    private final PurchaseInvoiceRepository
            repository;

    private final SupplierRepository
            supplierRepository;

    private final ProductRepository
            productRepository;

    private final ProductVariantRepository
            variantRepository;

    private final PurchaseInvoiceMapper
            mapper;

    private final BranchTenantGuard
            branchTenantGuard;

    private final ApValidationService
            validationService;

    private final DocumentNumberGeneratorService
            numberGeneratorService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public PurchaseInvoiceResponse create(
            CreatePurchaseInvoiceRequest request
    ) {

        branchTenantGuard.validate(
                request.getBranchId()
        );

        Supplier supplier =
                supplierRepository
                        .findActiveById(
                                request.getSupplierId(),
                                tenantId(),
                                request.getBranchId()
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Supplier not found"
                                )
                        );

        Optional<PurchaseInvoice> existing =
                repository
                        .findByTenantIdAndBranchIdAndSupplierInvoiceNumber(
                                tenantId(),
                                request.getBranchId(),
                                request.getSupplierInvoiceNumber()
                        );

        if (existing.isPresent()) {
            return mapper.toResponse(
                    existing.get()
            );
        }

        PurchaseInvoice invoice =
                PurchaseInvoice.builder()
                        .tenantId(tenantId())
                        .branchId(request.getBranchId())
                        .supplier(supplier)
                        .supplierInvoiceNumber(
                                request.getSupplierInvoiceNumber()
                        )
                        .documentNumber(
                                numberGeneratorService
                                        .nextPurchaseInvoiceNumber(
                                                request.getBranchId()
                                        )
                        )
                        .documentDate(
                                request.getInvoiceDate()
                        )
                        .postingDate(LocalDate.now())
                        .dueDate(request.getDueDate())
                        .documentStatus(
                                FinancialDocumentStatus.DRAFT
                        )
                        .postingStatus(
                                FinancialPostingStatus.UNPOSTED
                        )
                        .status(
                                PurchaseInvoiceStatus.DRAFT
                        )
                        .postingLifecycleStatus(
                                PurchaseInvoicePostingStatus.UNPOSTED
                        )
                        .posted(false)
                        .notes(request.getNotes())
                        .lines(new LinkedHashSet<>())
                        .build();

        BigDecimal subtotal =
                BigDecimal.ZERO;

        BigDecimal vatTotal =
                BigDecimal.ZERO;

        BigDecimal discountTotal =
                BigDecimal.ZERO;

        BigDecimal total =
                BigDecimal.ZERO;

        for (
                CreatePurchaseInvoiceLineRequest lineRequest :
                request.getLines()
        ) {

            Product product =
                    productRepository
                            .findByIdAndTenantIdAndBranchIdAndDeletedFalse(
                                    lineRequest.getProductId(),
                                    tenantId(),
                                    request.getBranchId()
                            ).orElseThrow(() ->
                            new IllegalArgumentException(
                                    "Product not found"
                            )
                    );

            ProductVariant variant = null;

            if (
                    lineRequest.getProductVariantId()
                            != null
            ) {

                variant =
                        variantRepository
                                .findByIdSafe(
                                        lineRequest.getProductVariantId(),
                                        false,
                                        tenantId(),
                                        request.getBranchId()
                                ).orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Variant not found"
                                )
                        );
            }

            BigDecimal qty =
                    BigDecimal.valueOf(
                            lineRequest.getQuantity()
                    );

            BigDecimal lineSubtotal =
                    lineRequest.getLineSubtotal();

            BigDecimal lineTotal =
                    lineRequest.getLineTotal();

            PurchaseInvoiceLine line =
                    PurchaseInvoiceLine.builder()
                            .purchaseInvoice(invoice)
                            .product(product)
                            .productVariant(variant)
                            .productNameSnapshot(
                                    product.getName()
                            )
                            .productSkuSnapshot(
                                    product.getSku()
                            )
                            .variantNameSnapshot(
                                    variant != null
                                            ? variant.getClassification()
                                            : "STANDARD"
                            )
                            .quantity(
                                    lineRequest.getQuantity()
                            )
                            .unitCost(
                                    lineRequest.getUnitCost()
                                            .setScale(
                                                    2,
                                                    RoundingMode.HALF_UP
                                            )
                            )
                            .discountAmount(
                                    lineRequest.getDiscountAmount()
                            )
                            .vatAmount(
                                    lineRequest.getVatAmount()
                            )
                            .vatInclusive(
                                    lineRequest.getVatInclusive()
                            )
                            .vatRate(
                                    lineRequest.getVatRate()
                            )
                            .lineSubtotal(
                                    lineSubtotal
                            )
                            .lineTotal(
                                    lineTotal
                            )
                            .build();

            invoice.getLines().add(line);

            subtotal =
                    subtotal.add(lineSubtotal);

            vatTotal =
                    vatTotal.add(
                            lineRequest.getVatAmount()
                    );

            discountTotal =
                    discountTotal.add(
                            lineRequest.getDiscountAmount()
                    );

            total =
                    total.add(lineTotal);
        }

        invoice.setSubtotal(subtotal);
        invoice.setVatAmount(vatTotal);
        invoice.setDiscountAmount(discountTotal);
        invoice.setTotalAmount(total);
        invoice.setOutstandingAmount(total);
        invoice.setAllocatedAmount(BigDecimal.ZERO);

        try {

            PurchaseInvoice saved =
                    repository.save(invoice);

            return mapper.toResponse(saved);

        } catch (
                DataIntegrityViolationException ex
        ) {

            existing =
                    repository
                            .findByTenantIdAndBranchIdAndSupplierInvoiceNumber(
                                    tenantId(),
                                    request.getBranchId(),
                                    request.getSupplierInvoiceNumber()
                            );

            if (existing.isPresent()) {
                return mapper.toResponse(
                        existing.get()
                );
            }

            throw new ExpectedConcurrencyException(
                    "Concurrent invoice creation detected"
            );
        }
    }

    @Transactional
    public PurchaseInvoiceResponse markApproved(
            UUID branchId,
            UUID invoiceId
    ) {

        PurchaseInvoice invoice =
                getManaged(branchId, invoiceId);

        validationService
                .validateDraftMutationAllowed(
                        invoice
                );

        invoice.setStatus(
                PurchaseInvoiceStatus.APPROVED
        );

        return mapper.toResponse(
                repository.save(invoice)
        );
    }

    @Transactional
    public PurchaseInvoiceResponse markCancelled(
            UUID branchId,
            UUID invoiceId,
            String reason
    ) {

        PurchaseInvoice invoice =
                getManaged(branchId, invoiceId);

        validationService
                .validateDraftMutationAllowed(
                        invoice
                );

        invoice.setCancelled(true);

        invoice.setStatus(
                PurchaseInvoiceStatus.CANCELLED
        );

        invoice.setDocumentStatus(
                FinancialDocumentStatus.CANCELLED
        );

        invoice.setNotes(
                reason
        );

        return mapper.toResponse(
                repository.save(invoice)
        );
    }

    @Transactional
    public void applyAllocation(
            PurchaseInvoice invoice,
            BigDecimal amount
    ) {
        BigDecimal outstanding =
                invoice.getOutstandingAmount()
                        .subtract(amount);

        BigDecimal allocated =
                invoice.getAllocatedAmount()
                        .add(amount);

        invoice.setOutstandingAmount(
                outstanding
        );

        invoice.setAllocatedAmount(
                allocated
        );

        boolean fullyAllocated =
                outstanding.compareTo(BigDecimal.ZERO) == 0;

        invoice.setFullyAllocated(
                fullyAllocated
        );

        invoice.setStatus(
                fullyAllocated
                        ? PurchaseInvoiceStatus.PAID
                        : PurchaseInvoiceStatus.PARTIALLY_PAID
        );

        validateAllocationInvariants(
                invoice
        );
    }

    @Transactional
    public void reverseAllocation(
            PurchaseInvoice invoice,
            BigDecimal amount
    ) {
        invoice.setAllocatedAmount(
                invoice.getAllocatedAmount()
                        .subtract(amount)
        );

        invoice.setOutstandingAmount(
                invoice.getOutstandingAmount()
                        .add(amount)
        );

        boolean fullyAllocated =
                invoice.getOutstandingAmount()
                        .compareTo(BigDecimal.ZERO) == 0;

        invoice.setFullyAllocated(
                fullyAllocated
        );

        invoice.setStatus(
                fullyAllocated
                        ? PurchaseInvoiceStatus.PAID
                        : (
                        invoice.getAllocatedAmount()
                                .compareTo(BigDecimal.ZERO) > 0
                                ? PurchaseInvoiceStatus.PARTIALLY_PAID
                                : PurchaseInvoiceStatus.POSTED
                )
        );

        validateAllocationInvariants(
                invoice
        );
    }

    private void validateAllocationInvariants(
            PurchaseInvoice invoice
    ) {
        BigDecimal variance =
                invoice.getAllocatedAmount()
                        .add(
                                invoice.getOutstandingAmount()
                        )
                        .subtract(
                                invoice.getTotalAmount()
                        )
                        .abs();

        if (variance.compareTo(new BigDecimal("0.01")) > 0) {
            throw new IllegalStateException(
                    "Invoice allocation invariant violation"
            );
        }
    }

    public PurchaseInvoiceResponse get(
            UUID branchId,
            UUID invoiceId
    ) {

        return mapper.toResponse(
                getManaged(
                        branchId,
                        invoiceId
                )
        );
    }

    public PurchaseInvoice getManaged(
            UUID branchId,
            UUID invoiceId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return repository
                .findByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        invoiceId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Purchase invoice not found"
                        )
                );
    }
}