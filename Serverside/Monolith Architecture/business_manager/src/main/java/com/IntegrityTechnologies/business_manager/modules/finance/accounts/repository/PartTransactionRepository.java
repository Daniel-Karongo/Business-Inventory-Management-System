package com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PartTransactionRepository extends JpaRepository<PartTransaction, UUID> {
    List<PartTransaction> findByRelatedModuleAndReferenceId(String relatedModule, UUID referenceId);
}