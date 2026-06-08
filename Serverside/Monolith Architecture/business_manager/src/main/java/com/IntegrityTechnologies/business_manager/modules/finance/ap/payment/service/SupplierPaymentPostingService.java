package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.AutoFundingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service.ApAllocationService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.SupplierPaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.mapper.SupplierPaymentMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierPaymentPostingService {

    private final SupplierPaymentService paymentService;
    private final SupplierPaymentRepository repository;
    private final SupplierPaymentMapper mapper;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final JournalEntryRepository journalEntryRepository;
    private final AutoFundingService autoFundingService;
    private final ApAllocationService allocationService;

    @Transactional
    public SupplierPaymentResponse post(
            UUID branchId,
            UUID paymentId
    ) {
        SupplierPayment payment =
                paymentService.getLocked(
                        branchId,
                        paymentId
                );

        if (payment.isPosted()) {
            return mapper.toResponse(
                    payment
            );
        }

        if (
                payment.getStatus()
                        != SupplierPaymentStatus.DRAFT
        ) {
            throw new IllegalStateException(
                    "Only draft payments may be posted"
            );
        }

        UUID tenantId =
                TenantContext.getTenantId();

        UUID apAccountId =
                accountingAccounts.get(
                        tenantId,
                        branchId,
                        "ACCOUNTS_PAYABLE"
                );

        UUID accountingEventId =
                UUID.nameUUIDFromBytes(
                        (
                                "SUPPLIER_PAYMENT:"
                                        + payment.getId()
                        ).getBytes()
                );

        autoFundingService.ensureFundingAvailable(
                branchId,
                payment.getFundingAccountId(),
                payment.getAmount(),
                payment.getPaymentDate(),
                payment.getDocumentNumber()
        );

        if (accountingFacade.isAlreadyPosted(
                "SUPPLIER_PAYMENT",
                payment.getId()
        )) {
            payment.setPosted(true);

            payment.setStatus(
                    SupplierPaymentStatus.POSTED
            );

            payment.setPostingStatus(
                    FinancialPostingStatus.POSTED
            );

            return mapper.toResponse(
                    repository.save(payment)
            );
        }

        accountingFacade.post(
                        AccountingEvent.builder()
                                .eventId(
                                        accountingEventId
                                )
                                .tenantId(
                                        tenantId
                                )
                                .branchId(
                                        branchId
                                )
                                .sourceModule(
                                        "SUPPLIER_PAYMENT"
                                )
                                .sourceId(
                                        payment.getId()
                                )
                                .reference(
                                        payment.getDocumentNumber()
                                )
                                .description(
                                        "Supplier payment posting"
                                )
                                .performedBy(
                                        currentUser()
                                )
                                .accountingDate(
                                        payment.getPaymentDate()
                                )
                                .entries(
                                        List.of(
                                                AccountingEvent.Entry
                                                        .builder()
                                                        .accountId(
                                                                apAccountId
                                                        )
                                                        .direction(
                                                                EntryDirection.DEBIT
                                                        )
                                                        .amount(
                                                                payment.getAmount()
                                                        )
                                                        .build(),
                                                AccountingEvent.Entry
                                                        .builder()
                                                        .accountId(
                                                                payment.getFundingAccountId()
                                                        )
                                                        .direction(
                                                                EntryDirection.CREDIT
                                                        )
                                                        .amount(
                                                                payment.getAmount()
                                                        )
                                                        .build()
                                        )
                                )
                                .build()
                );

        JournalEntry postedJournal =
                journalEntryRepository
                        .findByTenantIdAndSourceModuleAndSourceId(
                                tenantId,
                                "SUPPLIER_PAYMENT",
                                payment.getId()
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Posted journal not found"
                                )
                        );

        payment.setJournalEntryId(
                postedJournal.getId()
        );

        payment.setPosted(true);

        payment.setPostedAt(
                LocalDateTime.now()
        );

        payment.setPostedBy(
                currentUser()
        );

        payment.setStatus(
                SupplierPaymentStatus.POSTED
        );

        payment.setDocumentStatus(
                FinancialDocumentStatus.ACTIVE
        );

        payment.setPostingStatus(
                FinancialPostingStatus.POSTED
        );

        return mapper.toResponse(
                repository.save(payment)
        );
    }

    @Transactional
    public SupplierPaymentResponse reverse(
            UUID branchId,
            UUID paymentId,
            String reason
    ) {

        SupplierPayment payment =
                paymentService.getLocked(
                        branchId,
                        paymentId
                );

        if (payment.isReversed()) {
            throw new IllegalStateException(
                    "Payment already reversed"
            );
        }

        if (!payment.isPosted()) {
            throw new IllegalStateException(
                    "Only posted payments may be reversed"
            );
        }

        allocationService.reverseAllForPayment(
                branchId,
                payment,
                reason
        );

        accountingFacade.reverseJournal(
                payment.getJournalEntryId(),
                reason,
                currentUser()
        );

        JournalEntry reversedJournal =
                journalEntryRepository
                        .findByTenantIdAndId(
                                TenantContext.getTenantId(),
                                payment.getJournalEntryId()
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Original journal not found"
                                )
                        );

        payment.setReversalJournalEntryId(
                reversedJournal.getReversalJournalId()
        );

        payment.setReversed(true);

        payment.setReversedAt(
                LocalDateTime.now()
        );

        payment.setReversedBy(
                currentUser()
        );

        payment.setReversalReason(
                reason
        );

        /*
         * Reversed payments are financially inactive.
         * They must no longer contribute supplier credit.
         */
        payment.setUnappliedAmount(
                BigDecimal.ZERO
        );

        payment.setFullyAllocated(
                false
        );

        payment.setStatus(
                SupplierPaymentStatus.REVERSED
        );

        payment.setDocumentStatus(
                FinancialDocumentStatus.REVERSED
        );

        payment.setPostingStatus(
                FinancialPostingStatus.REVERSED
        );

        return mapper.toResponse(
                repository.save(payment)
        );
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