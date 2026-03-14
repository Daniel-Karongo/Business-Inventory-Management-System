package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DailySnapshotScheduler {

    private final BranchRepository branchRepo;
    private final DailySnapshotService snapshotService;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(cron = "${accounting.snapshot.cron}")
    public void runNightlySnapshots() {

        tenantExecutionService.forEachTenant(tenantId -> {

            int page = 0;
            Page<Branch> branches;

            do {

                branches =
                        branchRepo.findByTenantIdAndDeletedFalse(
                                tenantId,
                                PageRequest.of(page++, 50)
                        );

                for (Branch branch : branches.getContent()) {

                    snapshotService.snapshotBranch(branch.getId());

                }

            } while (branches.hasNext());

        });
    }
}