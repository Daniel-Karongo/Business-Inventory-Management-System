package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.security.BranchScoped;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JournalEntryRepository extends JpaRepository<JournalEntry, UUID> {

    Optional<JournalEntry> findTopByBranch_IdAndPostedTrueOrderByPostedAtDesc(UUID branchId);
    Optional<JournalEntry>
    findTopByBranch_IdAndPostedTrueAndIdNotOrderByPostedAtDesc(
            UUID branchId,
            UUID excludeId
    );

    Page<JournalEntry> findByBranch_Id(
            @BranchScoped UUID branchId,
            Pageable pageable
    );

    Page<JournalEntry> findByBranch_IdOrderByPostedAtDesc(
            @BranchScoped UUID branchId,
            Pageable pageable
    );

    Page<JournalEntry> findByBranch_IdOrderByPostedAtAsc(
            UUID branchId,
            Pageable pageable
    );

    boolean existsBySourceModuleAndSourceId(String sourceModule, UUID sourceId);

    Optional<JournalEntry> findBySourceModuleAndSourceId(String sourceModule, UUID sourceId);

    boolean existsByAccountingEventId(UUID eventId);

    long countByBranch_Id(UUID branchId);
    List<JournalEntry> findByReference(String reference);
    Page<JournalEntry> findAll(Pageable pageable);
}