package com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class FinancialProjectionReplayService {

    private final LedgerEntryRepository ledgerRepo;
    private final ApplicationEventPublisher publisher;
    private final BranchTenantGuard branchTenantGuard;
    private final VatLedgerProjectionRepository vatProjectionRepository;
    private final CorporateTaxLedgerProjectionRepository corporateProjectionRepository;
    private final AccountBalanceRepository accountBalanceRepository;

    @Transactional
    public void replayBranch(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        accountBalanceRepository.deleteBranchBalances(
                tenantId,
                branchId
        );

        vatProjectionRepository.deleteByTenantIdAndBranchId(
                tenantId,
                branchId
        );

        corporateProjectionRepository.deleteByTenantIdAndBranchId(
                tenantId,
                branchId
        );

        int page = 0;
        int size = 500;

        while (true) {

            List<LedgerEntry> entries =
                    ledgerRepo.streamBranchLedger(
                            tenantId,
                            branchId,
                            PageRequest.of(page, size)
                    );

            if (entries.isEmpty()) {
                break;
            }

            Map<UUID, List<LedgerEntry>> grouped =
                    entries.stream()
                            .collect(Collectors.groupingBy(
                                    e -> e.getJournalEntry().getId()
                            ));

            for (List<LedgerEntry> group : grouped.values()) {

                UUID journalId =
                        group.get(0).getJournalEntry().getId();

                List<LedgerEntryDTO> payload =
                        group.stream()
                                .map(e -> new LedgerEntryDTO(
                                        e.getAccount().getId(),
                                        e.getAccount().getType(),
                                        e.getAccount().getRole(),
                                        e.getDirection(),
                                        e.getAmount()
                                ))
                                .toList();

                publisher.publishEvent(
                        new JournalPostedEvent(
                                tenantId,
                                branchId,
                                journalId,
                                group.get(0).getJournalEntry().getPeriodId(),
                                group.get(0).getJournalEntry().getAccountingDate(),
                                group.get(0).getJournalEntry().getPostedAt(),
                                group.get(0).getJournalEntry().getSourceModule(),
                                payload
                        )
                );
            }

            page++;
        }
    }
}