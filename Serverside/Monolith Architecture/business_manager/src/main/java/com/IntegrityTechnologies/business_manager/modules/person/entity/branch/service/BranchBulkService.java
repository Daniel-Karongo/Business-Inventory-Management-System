package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.HashSet;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class BranchBulkService {

    private final BranchService branchService;
    private final BranchRepository branchRepository;

    @Transactional
    public BulkResult<BranchDTO> importBranches(
            BulkRequest<BranchBulkRow> request,
            Authentication authentication
    ) {

        BulkResult<BranchDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        Set<String> seenCodes = new HashSet<>();

        for (int i = 0; i < request.getItems().size(); i++) {
            BranchBulkRow row = request.getItems().get(i);
            int rowNumber = i + 1;

            try {
                validate(row);

                String code = normalizeCode(row.getBranchCode());

                // 🔥 1. Duplicate in same import
                if (!seenCodes.add(code)) {
                    throw new IllegalArgumentException(
                            "Duplicate branch code in import: " + code
                    );
                }

                // 🔥 2. Duplicate in database
                if (options.isSkipDuplicates()
                        && branchRepository.existsByBranchCodeIgnoreCase(code)) {

                    throw new IllegalArgumentException(
                            "Branch code already exists: " + code
                    );
                }

                BranchDTO dto = BranchDTO.builder()
                        .branchCode(code)
                        .name(row.getName())
                        .location(row.getLocation())
                        .phone(row.getPhone())
                        .email(row.getEmail())
                        .build();

                if (!options.isDryRun()) {
                    dto = branchService.create(dto, authentication);
                }

                result.addSuccess(dto);

            } catch (Exception ex) {
                result.addError(rowNumber, ex.getMessage());
            }
        }

        // 🔥 Ensure NOTHING persists on dry-run
        if (options.isDryRun()) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();
        }

        return result;
    }

    private void validate(BranchBulkRow row) {
        if (row.getBranchCode() == null || row.getBranchCode().isBlank()) {
            throw new IllegalArgumentException("branchCode is required");
        }
        if (row.getName() == null || row.getName().isBlank()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private String normalizeCode(String code) {
        return code == null ? null : code.trim().toUpperCase();
    }
}