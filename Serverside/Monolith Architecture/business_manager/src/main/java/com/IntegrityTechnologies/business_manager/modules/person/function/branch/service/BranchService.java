package com.IntegrityTechnologies.business_manager.modules.person.function.branch.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.dto.BranchDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.controller.CategoryController;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchService {

    private final BranchRepository branchRepository;
    private final PrivilegesChecker privilegesChecker;
    private final BranchAuditRepository branchAuditRepository;

    @Transactional
    public BranchDTO create(BranchDTO request, Authentication authentication) {

        if (branchRepository.existsByBranchCode(request.getBranchCode())) {
            throw new IllegalArgumentException("Branch code " + request.getBranchCode() + " already exists");
        }

        Branch branch = Branch.builder()
                .branchCode(request.getBranchCode())
                .name(request.getName())
                .location(request.getLocation())
                .phone(request.getPhone())
                .email(request.getEmail())
                .deleted(false)
                .build();

        branchRepository.save(branch);

        recordBranchAudit(branch, "CREATE", null, null, null, authentication, "Branch created");

        return toResponse(branch);
    }

    public List<BranchDTO> getAll(Boolean deleted) {
        return branchRepository.findAll()
                .stream()
                .filter(branch -> deleted == null ? true :
                        deleted == true ? branch.getDeleted() : !branch.getDeleted())
                .map(this::toResponse)
                .toList();
    }

    public BranchDTO getById(UUID id) {
        return branchRepository.findById(id)
                .map(this::toResponse)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch with id " + id + " not found"));
    }

    @Transactional
    public BranchDTO update(UUID id, BranchDTO request, Authentication authentication) {
        Branch branch = branchRepository.findById(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch with id " + id + " not found"));

        branch.setName(request.getName());
        branch.setLocation(request.getLocation());
        branch.setPhone(request.getPhone());
        branch.setEmail(request.getEmail());
        branch.setBranchCode(request.getBranchCode());

        recordBranchAudit(branch, "UPDATE", "entire entity", null, branch.toString(), authentication, "Branch entity updated");

        branchRepository.save(branch);

        return toResponse(branch);
    }

    @Transactional
    public void deleteBranch(UUID id, Boolean soft, Authentication authentication) {
        Branch branch;
        if(Boolean.TRUE.equals(soft)) {
            branch = branchRepository.findByIdAndDeletedFalse(id)
                    .orElseThrow(() ->
                            new EntityNotFoundException("Branch with id " + id + " not found or already deleted"));
            branch.setDeleted(true);
            branchRepository.save(branch);

            recordBranchAudit(branch, "SOFT_DELETE", "deleted", "false", "true", authentication, "Branch soft deleted");
        } else {
            branch = branchRepository.findById(id)
                    .orElseThrow(() ->
                            new EntityNotFoundException("Branch with id " + id + " not found or already deleted"));

            recordBranchAudit(branch, "HARD_DELETE", "entire entity", branch.toString(), null, authentication, "Branch deleted permanently");
            branchRepository.delete(branch);
        }
    }

    @Transactional
    public void restoreBranch(UUID id, Authentication authentication) {
        Branch branch = branchRepository.findByIdAndDeletedTrue(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch with id " + id + " not found, or not deleted"));
        branch.setDeleted(false);
        branchRepository.save(branch);

        recordBranchAudit(branch, "RESTORE", "deleted", "true", "false", authentication, "Branch created");
    }

    private BranchDTO toResponse(Branch branch) {
        return BranchDTO.builder()
                .id(branch.getId())
                .branchCode(branch.getBranchCode())
                .name(branch.getName())
                .location(branch.getLocation())
                .phone(branch.getPhone())
                .email(branch.getEmail())
                .deleted(branch.getDeleted())
                .build();
    }

    private void recordBranchAudit(
            Branch branch,
            String action,
            String field,
            String oldValue,
            String newValue,
            Authentication authentication,
            String reason
    ) {
        User user = privilegesChecker.getAuthenticatedUser(authentication);

        BranchAudit audit = BranchAudit.builder()
                .branchId(branch.getId())
                .branchName(branch.getName())
                .action(action)
                .fieldChanged(field)
                .oldValue(oldValue)
                .newValue(newValue)
                .reason(reason)
                .performedById(user.getId())
                .performedByUsername(user.getUsername())
                .build();
        branchAuditRepository.save(audit);
    }
}