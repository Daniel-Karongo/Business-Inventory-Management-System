package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.config.IntegrityProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class JournalIntegrityScheduler {

    private final JournalIntegrityService integrityService;
    private final IntegrityProperties properties;
    private final BranchRepository branchRepository;

    @Scheduled(cron = "${accounting.integrity.cron}")
    public void runIntegrityAudit() {

        if (!properties.isEnabled()) {
            return;
        }

        log.info("Running nightly journal integrity audit per branch...");

        int page = 0;
        Page<Branch> batch;

        do {
            batch = branchRepository.findByDeletedFalse(
                    PageRequest.of(page++, 200)
            );

            batch.forEach(branch -> {
                UUID branchId = branch.getId();

                var audit = integrityService.performAndPersistAudit(branchId);

                if (!audit.isValid()) {
                    log.error("Integrity broken for branch={} at journalId={}",
                            branchId,
                            audit.getBrokenAtJournalId());
                }
            });

        } while (batch.hasNext());
    }
}