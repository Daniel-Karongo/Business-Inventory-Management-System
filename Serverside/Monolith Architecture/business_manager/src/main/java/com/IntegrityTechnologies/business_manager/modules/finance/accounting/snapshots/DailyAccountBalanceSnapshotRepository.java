package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface DailyAccountBalanceSnapshotRepository
        extends JpaRepository<DailyAccountBalanceSnapshot, UUID> {

    List<DailyAccountBalanceSnapshot> findByBranchIdAndSnapshotDate(
            UUID branchId,
            LocalDate snapshotDate
    );

    List<DailyAccountBalanceSnapshot> findBySnapshotDate(LocalDate snapshotDate);

    Optional<DailyAccountBalanceSnapshot>
    findTopByBranchIdOrderBySnapshotDateDesc(UUID branchId);
}