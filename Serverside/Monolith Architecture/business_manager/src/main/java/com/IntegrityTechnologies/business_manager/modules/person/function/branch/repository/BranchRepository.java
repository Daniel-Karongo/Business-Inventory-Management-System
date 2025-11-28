package com.IntegrityTechnologies.business_manager.modules.person.function.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.Branch;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
import java.util.UUID;

public interface BranchRepository extends JpaRepository<Branch, UUID> {

    Optional<Branch> findByBranchCode(String branchCode);
    Optional<Branch> findByIdAndDeletedFalse(UUID id);
    Optional<Branch> findByIdAndDeletedTrue(UUID id);

    boolean existsByBranchCode(String branchCode);
}