package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service;

import com.IntegrityTechnologies.business_manager.exception.ExpectedConcurrencyException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository.SupplierPaymentAllocationRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.PaymentSettlementDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.mapper.SupplierPaymentMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.specification.SupplierPaymentSpecification;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.service.DocumentNumberGeneratorService;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierPaymentService {

    private static final Set<AccountRole> ALLOWED_FUNDING_ROLES =
            Set.of(
                    AccountRole.CASH,
                    AccountRole.BANK,
                    AccountRole.MPESA
            );

    private final SupplierPaymentRepository repository;

    private final BranchTenantGuard branchTenantGuard;

    private final SupplierPaymentMapper mapper;

    private final DocumentNumberGeneratorService numberGeneratorService;

    private final AccountRepository accountRepository;
    private final SupplierPaymentAllocationRepository allocationRepository;
    private final AccountBalanceRepository accountBalanceRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public SupplierPaymentResponse create(
            CreateSupplierPaymentRequest request
    ) {
        branchTenantGuard.validate(
                request.getBranchId()
        );

        validateFundingAccount(request);

        SupplierPayment payment =
                SupplierPayment.builder()
                        .tenantId(tenantId())
                        .branchId(request.getBranchId())
                        .documentNumber(
                                numberGeneratorService
                                        .nextSupplierPaymentNumber(
                                                request.getBranchId()
                                        )
                        )
                        .supplierId(
                                request.getSupplierId()
                        )
                        .fundingAccountId(
                                request.getFundingAccountId()
                        )
                        .amount(
                                request.getAmount()
                        )
                        .allocatedAmount(
                                BigDecimal.ZERO
                        )
                        .unappliedAmount(
                                request.getAmount()
                        )
                        .fullyAllocated(false)
                        .status(
                                SupplierPaymentStatus.DRAFT
                        )
                        .method(
                                request.getMethod()
                        )
                        .reference(
                                request.getReference()
                        )
                        .paymentDate(
                                request.getPaymentDate()
                        )
                        .paidAt(
                                LocalDateTime.now()
                        )
                        .paidBy(
                                currentUser()
                        )
                        .documentDate(
                                request.getPaymentDate()
                        )
                        .postingDate(
                                LocalDate.now()
                        )
                        .documentStatus(
                                FinancialDocumentStatus.DRAFT
                        )
                        .postingStatus(
                                FinancialPostingStatus.UNPOSTED
                        )
                        .posted(false)
                        .reversed(false)
                        .build();

        try {
            return mapper.toResponse(
                    repository.save(payment)
            );
        } catch (
                DataIntegrityViolationException ex
        ) {
            throw new ExpectedConcurrencyException(
                    "Concurrent supplier payment creation detected"
            );
        }
    }

    public Page<SupplierPaymentResponse> list(
            UUID branchId,
            UUID supplierId,
            SupplierPaymentStatus status,
            Pageable pageable
    ) {
        branchTenantGuard.validate(
                branchId
        );

        Page<SupplierPayment> results;

        if (supplierId != null && status != null) {

            results =
                    repository.findByTenantIdAndBranchIdAndSupplierIdAndStatusOrderByPaymentDateDesc(
                                    tenantId(),
                                    branchId,
                                    supplierId,
                                    status,
                                    pageable
                            );

        } else if (supplierId != null) {

            results =
                    repository.findByTenantIdAndBranchIdAndSupplierIdOrderByPaymentDateDesc(
                                    tenantId(),
                                    branchId,
                                    supplierId,
                                    pageable
                            );

        } else if (status != null) {

            results =
                    repository.findByTenantIdAndBranchIdAndStatusOrderByPaymentDateDesc(
                                    tenantId(),
                                    branchId,
                                    status,
                                    pageable
                            );

        } else {

            results =
                    repository.findByTenantIdAndBranchIdOrderByPaidAtDesc(
                                    tenantId(),
                                    branchId,
                                    pageable
                            );
        }

        return results.map(
                mapper::toResponse
        );
    }

    public Page<SupplierPaymentResponse> search(
            UUID branchId,
            SupplierPaymentSearchRequest request,
            Pageable pageable
    ) {
        branchTenantGuard.validate(
                branchId
        );

        return repository.findAll(
                        SupplierPaymentSpecification.search(
                                tenantId(),
                                branchId,
                                request
                        ),
                        pageable
                )
                .map(
                        mapper::toResponse
                );
    }

    public SupplierPaymentDetailsResponse details(
            UUID branchId,
            UUID paymentId
    ) {
        SupplierPayment payment =
                getManaged(
                        branchId,
                        paymentId
                );

        List<PaymentSettlementDto> allocations =
                allocationRepository
                        .findByTenantIdAndBranchIdAndSupplierPayment_Id(
                                tenantId(),
                                branchId,
                                paymentId
                        )
                        .stream()
                        .filter(a -> !a.isReversed())
                        .map(a ->
                                PaymentSettlementDto.builder()
                                        .invoiceId(
                                                a.getPurchaseInvoice().getId()
                                        )
                                        .billNumber(
                                                a.getPurchaseInvoice().getDocumentNumber()
                                        )
                                        .invoiceDate(
                                                a.getPurchaseInvoice().getDocumentDate()
                                        )
                                        .allocatedAmount(
                                                a.getAllocatedAmount()
                                        )
                                        .build()
                        )
                        .toList();

        return SupplierPaymentDetailsResponse
                .builder()
                .payment(
                        mapper.toResponse(
                                payment
                        )
                )
                .allocations(
                        allocations
                )
                .build();
    }
    
    public SupplierPayment getManaged(
            UUID branchId,
            UUID paymentId
    ) {
        branchTenantGuard.validate(
                branchId
        );

        return repository
                .findByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        paymentId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Supplier payment not found"
                        )
                );
    }

    public SupplierPayment getLocked(
            UUID branchId,
            UUID paymentId
    ) {
        branchTenantGuard.validate(
                branchId
        );

        return repository
                .findLockedByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        paymentId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Supplier payment not found"
                        )
                );
    }

    private void validateFundingAccount(
            CreateSupplierPaymentRequest request
    ) {
        Account account =
                accountRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                request.getBranchId(),
                                request.getFundingAccountId()
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Funding account not found"
                                )
                        );

        if (
                !ALLOWED_FUNDING_ROLES.contains(
                        account.getRole()
                )
        ) {
            throw new IllegalStateException(
                    "Invalid funding account role"
            );
        }
    }

    public List<FundingAccountResponse> getFundingAccounts(
            UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return accountRepository
                .findByTenantIdAndBranchId(
                        tenantId(),
                        branchId
                )
                .stream()
                .filter(a ->
                        ALLOWED_FUNDING_ROLES.contains(
                                a.getRole()
                        )
                )
                .map(a -> {

                    BigDecimal balance =
                            accountBalanceRepository
                                    .findByTenantIdAndAccount_IdAndBranch_Id(
                                            tenantId(),
                                            a.getId(),
                                            branchId
                                    )
                                    .map(AccountBalance::getBalance)
                                    .orElse(BigDecimal.ZERO);

                    return FundingAccountResponse
                            .builder()
                            .id(a.getId())
                            .code(a.getCode())
                            .name(a.getName())
                            .role(a.getRole())
                            .active(a.isActive())
                            .balance(balance)
                            .build();
                })
                .toList();
    }

    @Transactional
    public void applyAllocation(
            SupplierPayment payment,
            BigDecimal amount
    ) {
        BigDecimal unapplied =
                payment.getUnappliedAmount()
                        .subtract(amount);

        BigDecimal allocated =
                payment.getAllocatedAmount()
                        .add(amount);

        payment.setUnappliedAmount(
                unapplied
        );

        payment.setAllocatedAmount(
                allocated
        );

        boolean fullyAllocated =
                unapplied.compareTo(BigDecimal.ZERO) == 0;

        payment.setFullyAllocated(
                fullyAllocated
        );

        payment.setStatus(
                fullyAllocated
                        ? SupplierPaymentStatus.FULLY_ALLOCATED
                        : SupplierPaymentStatus.PARTIALLY_ALLOCATED
        );

        validateAllocationInvariants(
                payment
        );
    }

    @Transactional
    public void reverseAllocation(
            SupplierPayment payment,
            BigDecimal amount
    ) {
        payment.setAllocatedAmount(
                payment.getAllocatedAmount()
                        .subtract(amount)
        );

        payment.setUnappliedAmount(
                payment.getUnappliedAmount()
                        .add(amount)
        );

        boolean fullyAllocated =
                payment.getUnappliedAmount()
                        .compareTo(BigDecimal.ZERO) == 0;

        payment.setFullyAllocated(
                fullyAllocated
        );

        payment.setStatus(
                fullyAllocated
                        ? SupplierPaymentStatus.FULLY_ALLOCATED
                        : (
                        payment.getAllocatedAmount()
                                .compareTo(BigDecimal.ZERO) > 0
                                ? SupplierPaymentStatus.PARTIALLY_ALLOCATED
                                : SupplierPaymentStatus.POSTED
                )
        );

        validateAllocationInvariants(
                payment
        );
    }

    private void validateAllocationInvariants(
            SupplierPayment payment
    ) {
        if (
                payment.getAllocatedAmount()
                        .add(payment.getUnappliedAmount())
                        .compareTo(payment.getAmount()) != 0
        ) {
            throw new IllegalStateException(
                    "Payment allocation invariant violation"
            );
        }
    }

    private String currentUser() {
        var auth =
                SecurityContextHolder
                        .getContext()
                        .getAuthentication();

        return auth != null
                ? auth.getName()
                : "SYSTEM";
    }
}