package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierPaymentService {

    private final SupplierPaymentRepository paymentRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public SupplierPayment pay(
            UUID branchId,
            UUID supplierId,
            BigDecimal amount,
            String method,
            String reference
    ) {

        branchTenantGuard.validate(branchId);

        if (amount == null || amount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException(
                    "Amount must be greater than zero"
            );
        }

        UUID creditAccount =
                resolvePaymentAccount(method, branchId);

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("SUPPLIER_PAYMENT")
                        .sourceId(UUID.randomUUID())
                        .reference(reference)
                        .description("Supplier payment via " + method)
                        .performedBy(currentUser())
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .accountingDate(LocalDate.now())
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(
                                                accounts.get(
                                                        tenantId(),
                                                        branchId,
                                                        AccountRole.ACCOUNTS_PAYABLE
                                                )
                                        )
                                        .direction(EntryDirection.DEBIT)
                                        .amount(amount)
                                        .build(),

                                AccountingEvent.Entry.builder()
                                        .accountId(creditAccount)
                                        .direction(EntryDirection.CREDIT)
                                        .amount(amount)
                                        .build()
                        ))
                        .build()
        );

        SupplierPayment payment =
                SupplierPayment.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .supplierId(supplierId)
                        .amount(amount)
                        .method(method.toUpperCase())
                        .reference(reference)
                        .paidAt(LocalDateTime.now())
                        .paidBy(currentUser())
                        .build();

        return paymentRepository.save(payment);
    }

    private UUID resolvePaymentAccount(
            String method,
            UUID branchId
    ) {

        return switch (method.toUpperCase()) {

            case "CASH" ->
                    accounts.get(
                            tenantId(),
                            branchId,
                            AccountRole.CASH
                    );

            case "BANK" ->
                    accounts.get(
                            tenantId(),
                            branchId,
                            AccountRole.BANK
                    );

            case "MPESA" ->
                    accounts.get(
                            tenantId(),
                            branchId,
                            AccountRole.MPESA
                    );

            default ->
                    throw new IllegalArgumentException(
                            "Unsupported payment method"
                    );
        };
    }

    private String currentUser() {

        var auth =
                SecurityContextHolder.getContext().getAuthentication();

        return auth != null
                ? auth.getName()
                : "SYSTEM";
    }

    public Page<SupplierPayment> list(
            UUID branchId,
            Pageable pageable
    ) {

        branchTenantGuard.validate(branchId);

        return paymentRepository
                .findByTenantIdAndBranchIdOrderByPaidAtDesc(
                        tenantId(),
                        branchId,
                        pageable
                );
    }
}