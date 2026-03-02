package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranchId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface UserBranchRepository
        extends JpaRepository<UserBranch, UserBranchId> {

    List<UserBranch> findByBranchId(UUID branchId);

    List<UserBranch> findByUserId(UUID userId);

    @Modifying
    @Query("delete from UserBranch ub where ub.user.id = :userId")
    void deleteByUserId(UUID userId);

    @Modifying
    @Query("delete from UserBranch ub where ub.branch.id = :branchId")
    void deleteByBranchId(UUID branchId);
}