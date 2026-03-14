package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.*;

public interface JournalEntryRepository extends JpaRepository<JournalEntry, UUID> {

    /* =========================================================
       CHAIN LOOKUP
    ========================================================= */

    Optional<JournalEntry>
    findTopByTenantIdAndBranchIdAndPostedTrueOrderByPostedAtDesc(
            UUID tenantId,
            UUID branchId
    );

    Optional<JournalEntry>
    findTopByTenantIdAndBranchIdAndPostedTrueAndIdNotOrderByPostedAtDescIdDesc(
            UUID tenantId,
            UUID branchId,
            UUID excludeId
    );

    /* =========================================================
       JOURNAL PAGING
    ========================================================= */

    Page<JournalEntry>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<JournalEntry>
    findByTenantIdAndBranchIdOrderByPostedAtDesc(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<JournalEntry>
    findByTenantIdAndBranchIdAndPostedTrueOrderByPostedAtAsc(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    /* =========================================================
       SOURCE DUPLICATE PROTECTION
    ========================================================= */

    boolean existsByTenantIdAndSourceModuleAndSourceId(
            UUID tenantId,
            String sourceModule,
            UUID sourceId
    );

    Optional<JournalEntry>
    findByTenantIdAndSourceModuleAndSourceId(
            UUID tenantId,
            String sourceModule,
            UUID sourceId
    );

    /* =========================================================
       SAFE REFERENCE SEARCH
    ========================================================= */

    List<JournalEntry>
    findByTenantIdAndBranchIdAndReference(
            UUID tenantId,
            UUID branchId,
            String reference
    );

    /* =========================================================
       EVENT DUPLICATE PROTECTION
    ========================================================= */

    boolean existsByTenantIdAndAccountingEventId(
            UUID tenantId,
            UUID eventId
    );

    /* =========================================================
       METRICS
    ========================================================= */

    long countByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );

    /* =========================================================
       SAFE FETCH
    ========================================================= */

    Optional<JournalEntry>
    findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );
}