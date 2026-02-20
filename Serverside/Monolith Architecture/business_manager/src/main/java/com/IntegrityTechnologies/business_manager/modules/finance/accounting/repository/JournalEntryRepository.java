package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JournalEntryRepository extends JpaRepository<JournalEntry, UUID> {
    List<JournalEntry> findAllByOrderByPostedAtDesc();
    boolean existsBySourceModuleAndSourceId(String sourceModule, UUID sourceId);
    Optional<JournalEntry> findBySourceModuleAndSourceId(String sourceModule, UUID sourceId);
    List<JournalEntry> findByReference(String reference);
}