package com.IntegrityTechnologies.business_manager.modules.person.branch.document.repository;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocument;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface BranchDocumentRepository extends JpaRepository<BranchDocument,UUID> {

    List<BranchDocument> findByTenantIdAndBranchIdOrderByUploadedAtDesc(
            UUID tenantId,
            UUID branchId
    );

    List<BranchDocument> findByTenantIdAndBranchIdAndDeletedOrderByUploadedAtDesc(
            UUID tenantId,
            UUID branchId,
            boolean deleted
    );

    Optional<BranchDocument> findByTenantIdAndBranchIdAndFileName(
            UUID tenantId,
            UUID branchId,
            String fileName
    );

    Optional<BranchDocument> findByTenantIdAndBranchIdAndLogoTrueAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        select d
        from BranchDocument d
        where d.tenantId=:tenantId
        and d.branchId=:branchId
        and d.deleted=false
        and d.documentType=:type
    """)
    List<BranchDocument> findActiveByType(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("type") BranchDocumentType type
    );

    Page<BranchDocument> findByTenantId(
            UUID tenantId,
            Pageable pageable
    );

    @Modifying
    @Query("""
        update BranchDocument d
        set d.logo=false
        where d.tenantId=:tenantId
        and d.branchId=:branchId
        and d.logo=true
    """)
    void clearExistingLogo(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    List<BranchDocument> findByTenantIdAndBranchIdAndDeleted(
            UUID tenantId,
            UUID branchId,
            boolean deleted);
}