package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BranchService {

    private final BranchRepository branchRepository;
    private final PrivilegesChecker privilegesChecker;
    private final BranchAuditRepository branchAuditRepository;
    private final UserRepository userRepository;
    private final DepartmentRepository departmentRepository;

    @Transactional
    public BranchDTO create(BranchDTO request, Authentication authentication) {

        if (branchRepository.existsByBranchCode(request.getBranchCode())) {
            throw new IllegalArgumentException("Branch code " + request.getBranchCode() + " already exists");
        }

        Set<User> users = new HashSet<>();
        if (request.getUserIds() != null && !request.getUserIds().isEmpty()) {
            users = request.getUserIds().stream()
                    .map(userId -> userRepository.findByIdAndDeletedFalse(userId)
                            .orElseThrow(() -> new RuntimeException("User not found: " + userId)))
                    .collect(Collectors.toSet());
        }

        Set<Department> departments = new HashSet<>();
        if (request.getDepartmentIds() != null && !request.getDepartmentIds().isEmpty()) {
            departments = request.getDepartmentIds().stream()
                    .map(departmentId -> departmentRepository.findByIdAndDeletedFalse(departmentId)
                            .orElseThrow(() -> new RuntimeException("Department not found: " + departmentId)))
                    .collect(Collectors.toSet());
        }

        Branch branch = Branch.builder()
                .users(users)
                .departments(departments)
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
        branch.setUsers((request.getUserIds().stream().map(userId -> userRepository.findByIdAndDeletedFalse(userId).get()).collect(Collectors.toSet())));
        branch.setDepartments(request.getDepartmentIds().stream().map(departmentId -> departmentRepository.findByIdAndDeletedFalse(departmentId).get()).collect(Collectors.toSet()));

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
                .users(branch.getUsers().stream().map(user -> MinimalUserDTO.from(user)).collect(Collectors.toSet()))
                .departments(branch.getDepartments().stream().map(department -> DepartmentMinimalDTO.from(department)).collect(Collectors.toSet()))
                .branchCode(branch.getBranchCode())
                .name(branch.getName())
                .location(branch.getLocation())
                .phone(branch.getPhone())
                .email(branch.getEmail())
                .deleted(branch.getDeleted())
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