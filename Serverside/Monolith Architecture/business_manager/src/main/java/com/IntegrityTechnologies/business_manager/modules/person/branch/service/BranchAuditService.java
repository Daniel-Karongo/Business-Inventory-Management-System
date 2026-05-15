package com.IntegrityTechnologies.business_manager.modules.person.branch.service;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchAuditService {

    private final BranchAuditRepository branchAuditRepository;

    @Transactional(readOnly = true)
    public PageWrapper<BranchAuditDTO> getAudits(
            UUID branchId,
            int page,
            int size
    ) {

        if (branchId == null) {

            throw new EntityNotFoundException(
                    "Branch ID is required"
            );
        }

        Pageable pageable =
                PageRequest.of(page, size);

        return new PageWrapper<>(
                branchAuditRepository
                        .findByTenantIdAndBranchIdOrderByTimestampDesc(
                                TenantContext.getTenantId(),
                                branchId,
                                pageable
                        )
                        .map(
                                audit ->
                                        BranchAuditDTO.builder()
                                                .id(audit.getId())
                                                .branchId(audit.getBranchId())
                                                .branchName(audit.getBranchName())
                                                .action(audit.getAction())
                                                .fieldChanged(audit.getFieldChanged())
                                                .oldValue(audit.getOldValue())
                                                .newValue(audit.getNewValue())
                                                .reason(audit.getReason())
                                                .performedById(audit.getPerformedById())
                                                .performedByUsername(audit.getPerformedByUsername())
                                                .timestamp(audit.getTimestamp())
                                                .build()
                        )
        );
    }
}