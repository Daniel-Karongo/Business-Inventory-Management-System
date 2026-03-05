package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranchId;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserBranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BranchService {

    private final BranchRepository branchRepository;
    private final PrivilegesChecker privilegesChecker;
    private final BranchAuditRepository branchAuditRepository;
    private final UserRepository userRepository;
    private final DepartmentRepository departmentRepository;
    private final UserBranchRepository userBranchRepository;
    private final UserDepartmentRepository userDepartmentRepository;
    private final BranchAccountingSettingsRepository branchAccountingSettingsRepository;

    @Transactional
    public BranchDTO create(BranchDTO request, Authentication authentication) {

        if (branchRepository.existsByBranchCodeIgnoreCase(request.getBranchCode())) {
            throw new IllegalArgumentException("Branch code " + request.getBranchCode() + " already exists");
        }

        Branch branch = Branch.builder()
                .branchCode(request.getBranchCode())
                .name(request.getName())
                .location(request.getLocation())
                .phone(PhoneAndEmailNormalizer.normalizePhone(request.getPhone()))
                .email(request.getEmail())
                .deleted(false)
                .build();

        branch = branchRepository.save(branch);

        branchAccountingSettingsRepository.save(
                BranchAccountingSettings.builder()
                        .branchId(branch.getId())
                        .revenueRecognitionMode(
                                RevenueRecognitionMode.DELIVERY
                        )
                        .build()
        );

        // Assign users via UserBranch
        if (request.getUserIds() != null) {
            for (UUID userId : request.getUserIds()) {
                User user = userRepository.findByIdAndDeletedFalse(userId)
                        .orElseThrow(() -> new RuntimeException("User not found: " + userId));

                UserBranch relation = UserBranch.builder()
                        .id(new UserBranchId(user.getId(), branch.getId()))
                        .user(user)
                        .branch(branch)
                        .primaryBranch(false)
                        .build();

                userBranchRepository.save(relation);
            }
        }

        recordBranchAudit(branch, "CREATE", null, null, null, authentication, "Branch created");

        return toResponse(branch);
    }

    public List<BranchDTO> getAll(Boolean deleted) {

        List<Branch> branches;

        if (deleted == null) {
            branches = branchRepository.findAll();
        } else if (deleted) {
            branches = branchRepository.findByDeleted(true);
        } else {
            branches = branchRepository.findByDeletedFalse();
        }

        return branches.stream()
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

        Branch branch = branchRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch with id " + id + " not found"));

        // ===== Field-by-field change tracking =====
        if (!Objects.equals(branch.getName(), request.getName())) {
            recordBranchAudit(branch, "UPDATE", "name",
                    branch.getName(), request.getName(),
                    authentication, "Branch name updated");
            branch.setName(request.getName());
        }

        if (!Objects.equals(branch.getLocation(), request.getLocation())) {
            recordBranchAudit(branch, "UPDATE", "location",
                    branch.getLocation(), request.getLocation(),
                    authentication, "Branch location updated");
            branch.setLocation(request.getLocation());
        }

        String normalizedPhone = PhoneAndEmailNormalizer.normalizePhone(request.getPhone());
        if (!Objects.equals(branch.getPhone(), normalizedPhone)) {
            recordBranchAudit(branch, "UPDATE", "phone",
                    branch.getPhone(), normalizedPhone,
                    authentication, "Branch phone updated");
            branch.setPhone(normalizedPhone);
        }

        if (!Objects.equals(branch.getEmail(), request.getEmail())) {
            recordBranchAudit(branch, "UPDATE", "email",
                    branch.getEmail(), request.getEmail(),
                    authentication, "Branch email updated");
            branch.setEmail(request.getEmail());
        }

        // ❌ branchCode intentionally NOT updatable

        // ===== Users update safely =====
        if (request.getUserIds() != null) {

            // Clear old relations
            userBranchRepository.deleteAll(
                    userBranchRepository.findAll().stream()
                            .filter(ub -> ub.getBranch().getId().equals(branch.getId()))
                            .toList()
            );

            for (UUID userId : request.getUserIds()) {

                User user = userRepository.findByIdAndDeletedFalse(userId)
                        .orElseThrow(() -> new EntityNotFoundException("User not found: " + userId));

                UserBranch relation = UserBranch.builder()
                        .id(new UserBranchId(user.getId(), branch.getId()))
                        .user(user)
                        .branch(branch)
                        .primaryBranch(false)
                        .build();

                userBranchRepository.save(relation);
            }
        }

        // ===== Departments update safely =====
        if (request.getDepartmentIds() != null) {
            Set<Department> departments = request.getDepartmentIds().stream()
                    .map(deptId -> departmentRepository.findByIdAndDeletedFalse(deptId)
                            .orElseThrow(() -> new EntityNotFoundException("Department not found: " + deptId)))
                    .collect(Collectors.toSet());

            branch.setDepartments(departments);
        }

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
            userBranchRepository.deleteByBranchId(id);
            departmentRepository.findByBranch_Id(id)
                    .forEach(d ->
                            userDepartmentRepository.deleteByDepartmentId(d.getId())
                    );

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

        recordBranchAudit(branch, "RESTORE", "deleted", "true", "false", authentication, "Branch restored");
    }

    private BranchDTO toResponse(Branch branch) {

        Set<MinimalUserDTO> users =
                userBranchRepository.findByBranchId(branch.getId())
                        .stream()
                        .map(ub -> MinimalUserDTO.from(ub.getUser()))
                        .collect(Collectors.toSet());

        Set<DepartmentMinimalDTO> departments =
                departmentRepository.findByBranch_Id(branch.getId())
                        .stream()
                        .map(DepartmentMinimalDTO::from)
                        .collect(Collectors.toSet());

        return BranchDTO.builder()
                .id(branch.getId())
                .branchCode(branch.getBranchCode())
                .name(branch.getName())
                .location(branch.getLocation())
                .phone(branch.getPhone())
                .email(branch.getEmail())
                .createdAt(branch.getCreatedAt())
                .deleted(branch.getDeleted())
                .users(users)
                .departments(departments)
                .build();
    }

    public void recordBranchAudit(
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