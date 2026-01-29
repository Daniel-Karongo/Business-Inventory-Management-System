package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.UserBranchScope;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface UserBranchScopeRepository extends JpaRepository<UserBranchScope, UUID> {

    List<UserBranchScope> findByUserIdAndActiveTrue(UUID userId);

    boolean existsByUserIdAndBranchIdAndActiveTrue(UUID userId, UUID branchId);
}