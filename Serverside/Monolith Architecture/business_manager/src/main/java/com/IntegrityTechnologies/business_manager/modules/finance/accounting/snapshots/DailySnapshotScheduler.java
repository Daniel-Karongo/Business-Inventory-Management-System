package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
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

    @Scheduled(cron = "${accounting.snapshot.cron}")
    public void runNightlySnapshots() {

        int page = 0;

        Page<Branch> branches;

        do {

            branches =
                    branchRepo.findByDeletedFalse(
                            PageRequest.of(page++, 50)
                    );

            branches.forEach(branch ->
                    snapshotService.snapshotBranch(branch.getId())
            );

        } while (branches.hasNext());
    }
}