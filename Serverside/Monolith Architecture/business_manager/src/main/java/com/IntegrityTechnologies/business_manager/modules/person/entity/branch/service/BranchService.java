package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranchId;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserBranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionGuard;
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
    private final BranchAuditRepository branchAuditRepository;
    private final UserRepository userRepository;
    private final DepartmentRepository departmentRepository;
    private final UserBranchRepository userBranchRepository;
    private final UserDepartmentRepository userDepartmentRepository;
    private final BranchAccountingSettingsRepository branchAccountingSettingsRepository;
    private final BranchChartOfAccountsService coaService;
    private final PrivilegesChecker privilegesChecker;
    private final SubscriptionGuard subscriptionGuard;


    /* =====================================================
       CREATE
    ===================================================== */

    @Transactional
    public BranchDTO create(BranchDTO request, Authentication authentication) {

        subscriptionGuard.checkBranchLimit();

        if (branchRepository.existsByBranchCodeIgnoreCase(request.getBranchCode())) {
            throw new IllegalArgumentException(
                    "Branch code already exists: " + request.getBranchCode()
            );
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

        bootstrapAccounting(branch);

        assignUsers(branch, request.getUserIds());

        recordAudit(branch, "CREATE", null, null, null, authentication, "Branch created");

        return toDTO(branch);
    }

    /* =====================================================
       READ
    ===================================================== */

    @Transactional(readOnly = true)
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
                .map(this::toDTO)
                .toList();
    }

    @Transactional(readOnly = true)
    public BranchDTO getById(UUID id) {

        Branch branch = branchRepository.findById(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch not found: " + id));

        return toDTO(branch);
    }

    /* =====================================================
       UPDATE
    ===================================================== */

    @Transactional
    public BranchDTO update(UUID id, BranchDTO request, Authentication authentication) {

        Branch branch = branchRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch not found: " + id));

        updateField(branch, "name", branch.getName(), request.getName(),
                v -> branch.setName(request.getName()), authentication);

        updateField(branch, "location", branch.getLocation(), request.getLocation(),
                v -> branch.setLocation(request.getLocation()), authentication);

        String phone = PhoneAndEmailNormalizer.normalizePhone(request.getPhone());

        updateField(branch, "phone", branch.getPhone(), phone,
                v -> branch.setPhone(phone), authentication);

        updateField(branch, "email", branch.getEmail(), request.getEmail(),
                v -> branch.setEmail(request.getEmail()), authentication);

        updateUsers(branch, request.getUserIds());

        updateDepartments(branch, request.getDepartmentIds());

        branchRepository.save(branch);

        return toDTO(branch);
    }


    /* =====================================================
       DELETE
    ===================================================== */

    @Transactional
    public void deleteBranch(UUID id, Boolean soft, Authentication authentication) {

        Branch branch = branchRepository.findById(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Branch not found: " + id));

        if (Boolean.TRUE.equals(soft)) {

            branch.setDeleted(true);
            branchRepository.save(branch);

            recordAudit(branch, "SOFT_DELETE", "deleted",
                    "false", "true", authentication, "Branch soft deleted");

        } else {

            userBranchRepository.deleteByBranchId(id);

            departmentRepository.findByBranch_Id(id)
                    .forEach(d ->
                            userDepartmentRepository.deleteByDepartmentId(d.getId()));

            branchRepository.delete(branch);

            recordAudit(branch, "HARD_DELETE", null,
                    branch.toString(), null, authentication, "Branch permanently deleted");
        }
    }

    /* =====================================================
       RESTORE
    ===================================================== */

    @Transactional
    public void restoreBranch(UUID id, Authentication authentication) {

        Branch branch = branchRepository.findByIdAndDeletedTrue(id)
                .orElseThrow(() ->
                        new EntityNotFoundException("Deleted branch not found: " + id));

        branch.setDeleted(false);

        branchRepository.save(branch);

        recordAudit(branch, "RESTORE", "deleted",
                "true", "false", authentication, "Branch restored");
    }

    /* =====================================================
       INTERNAL HELPERS
    ===================================================== */

    private void bootstrapAccounting(Branch branch) {

        branchAccountingSettingsRepository.save(
                BranchAccountingSettings.builder()
                        .branchId(branch.getId())
                        .revenueRecognitionMode(
                                RevenueRecognitionMode.DELIVERY
                        )
                        .build()
        );

        coaService.seedForBranch(branch.getId());
    }

    private void assignUsers(Branch branch, List<UUID> userIds) {

        if (userIds == null || userIds.isEmpty()) return;

        List<User> users = userRepository.findAllById(userIds);

        for (User user : users) {

            UserBranch relation = UserBranch.builder()
                    .id(new UserBranchId(user.getId(), branch.getId()))
                    .user(user)
                    .branch(branch)
                    .primaryBranch(false)
                    .build();

            userBranchRepository.save(relation);
        }
    }

    private void updateUsers(Branch branch, List<UUID> userIds) {

        if (userIds == null) return;

        userBranchRepository.deleteByBranchId(branch.getId());

        assignUsers(branch, userIds);
    }

    private void updateDepartments(Branch branch, List<UUID> departmentIds) {

        if (departmentIds == null) return;

        Set<Department> departments = departmentIds.stream()
                .map(id ->
                        departmentRepository.findByIdAndDeletedFalse(id)
                                .orElseThrow(() ->
                                        new EntityNotFoundException("Department not found: " + id)))
                .collect(Collectors.toSet());

        branch.setDepartments(departments);
    }


    private void updateField(
            Branch branch,
            String field,
            String oldValue,
            String newValue,
            java.util.function.Consumer<String> updater,
            Authentication authentication
    ) {

        if (!Objects.equals(oldValue, newValue)) {

            updater.accept(newValue);

            recordAudit(branch, "UPDATE",
                    field, oldValue, newValue,
                    authentication, "Field updated");
        }
    }

    private BranchDTO toDTO(Branch branch) {

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

    private void recordAudit(
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