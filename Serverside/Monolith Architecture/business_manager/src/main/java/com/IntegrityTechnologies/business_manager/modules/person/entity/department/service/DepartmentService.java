package com.IntegrityTechnologies.business_manager.modules.person.entity.department.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service.BranchService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.mapper.DepartmentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserDepartment;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserDepartmentId;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.category.controller.CategoryController;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DepartmentService {

    private final DepartmentRepository departmentRepository;
    private final UserRepository userRepository;
    private final DepartmentAuditRepository departmentAuditRepository;
    private final PrivilegesChecker privilegesChecker;
    private final BranchRepository branchRepository;
    private final BranchService branchService;
    private final DepartmentMapper departmentMapper;
    private final UserDepartmentRepository userDepartmentRepository;

    @Transactional
    public DepartmentDTO create(DepartmentDTO dto, Authentication authentication) {

        if (departmentRepository.existsByNameIgnoreCaseAndBranch_Id(
                dto.getName(),
                dto.getBranchId()
        )) {
            throw new IllegalArgumentException(
                    "Department with name '" + dto.getName()
                            + "' already exists in this branch");
        }

        Branch branch = branchRepository.findByIdAndDeletedFalse(dto.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        Department department = Department.builder()
                .name(dto.getName())
                .description(dto.getDescription())
                .branch(branch)
                .rollcallStartTime(dto.getRollcallStartTime())
                .gracePeriodMinutes(dto.getGracePeriodMinutes())
                .deleted(false)
                .build();

        department = departmentRepository.save(department);

        assignUsersToDepartment(dto.getHeadIds(), department, DepartmentMembershipRole.HEAD);
        assignUsersToDepartment(dto.getMemberIds(), department, DepartmentMembershipRole.MEMBER);

        logAudit(department, "CREATE", null, dto.toString(), authentication, null);

        return departmentMapper.toDTO(department);
    }

    private void assignUsersToDepartment(
            Set<UUID> userIds,
            Department department,
            DepartmentMembershipRole role
    ) {

        if (userIds == null) return;

        for (UUID userId : userIds) {

            User user = userRepository.findByIdAndDeletedFalse(userId)
                    .orElseThrow(() ->
                            new IllegalArgumentException("User not found: " + userId));

            UserDepartment relation = UserDepartment.builder()
                    .id(new UserDepartmentId(user.getId(), department.getId()))
                    .user(user)
                    .department(department)
                    .role(role)
                    .build();

            userDepartmentRepository.save(relation);
        }
    }

    @Transactional
    public DepartmentDTO updateDepartment(UUID departmentId,
                                          DepartmentDTO dto,
                                          Authentication authentication) {

        Department department = getById(departmentId);

        if (!department.getName().equalsIgnoreCase(dto.getName()) &&
                departmentRepository.existsByNameIgnoreCase(dto.getName())) {
            throw new IllegalArgumentException(
                    "Department with the name '" + dto.getName() + "' already exists");
        }

        Map<String, String> oldValues = mapDepartment(department);

        department.setName(dto.getName());
        department.setDescription(dto.getDescription());
        department.setRollcallStartTime(dto.getRollcallStartTime());
        department.setGracePeriodMinutes(dto.getGracePeriodMinutes());

        departmentRepository.save(department);

        // 🔹 Remove old user relations
        userDepartmentRepository.deleteAll(
                userDepartmentRepository.findByDepartmentId(department.getId())
        );

        // 🔹 Reassign users
        assignUsersToDepartment(dto.getHeadIds(), department, DepartmentMembershipRole.HEAD);
        assignUsersToDepartment(dto.getMemberIds(), department, DepartmentMembershipRole.MEMBER);

        Map<String, String> newValues = mapDepartment(department);

        if (!oldValues.equals(newValues)) {
            logAudit(department,
                    "UPDATE",
                    oldValues.toString(),
                    newValues.toString(),
                    authentication,
                    "Department updated");
        }

        return departmentMapper.toDTO(department);
    }

    @Transactional
    public Department updateExisting(
            Department existing,
            DepartmentDTO dto,
            Authentication authentication
    ) {

        Map<String, String> oldValues = mapDepartment(existing);

        existing.setDescription(dto.getDescription());
        existing.setRollcallStartTime(dto.getRollcallStartTime());
        existing.setGracePeriodMinutes(dto.getGracePeriodMinutes());

        departmentRepository.save(existing);

        userDepartmentRepository.deleteAll(
                userDepartmentRepository.findByDepartmentId(existing.getId())
        );

        assignUsersToDepartment(dto.getHeadIds(), existing, DepartmentMembershipRole.HEAD);
        assignUsersToDepartment(dto.getMemberIds(), existing, DepartmentMembershipRole.MEMBER);

        logAudit(
                existing,
                "UPDATE",
                oldValues.toString(),
                mapDepartment(existing).toString(),
                authentication,
                "Bulk import update"
        );

        return existing;
    }



    public List<DepartmentMinimalDTO> getAllDepartmentsForUser(UUID userId) {
        Set<Department> departments = departmentRepository.findDepartmentsByUserId(userId);
        return departments.stream().map(department -> DepartmentMinimalDTO.from(department)).toList();
    }

    public Department getById(UUID id) {
        return departmentRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Department not found"));
    }

    // --- Get all active departments ---
    public List<DepartmentDTO> getAllDepartments(Boolean deleted) {
        return departmentRepository.findAll().stream()
                .filter(
                        d ->
                                deleted == null
                                        ? true
                                        : deleted
                                        ? Boolean.TRUE.equals(d.isDeleted())
                                        : Boolean.FALSE.equals(d.isDeleted())
                )
                .map(d -> departmentMapper.toDTO(d))
                .collect(Collectors.toList());
    }

    public List<DepartmentAudit> getAllDepartmentsAudits() {
        return departmentAuditRepository.findAllByOrderByTimestampDesc();
    }

    public List<DepartmentAudit> getDepartmentAudits(UUID id) {
        return departmentAuditRepository.findByDepartmentIdOrderByTimestampDesc(id);
    }

    public List<DepartmentAudit> getDepartmentAuditsByPerformer(UUID id) {
        return departmentAuditRepository.findByPerformedByIdOrderByTimestampDesc(id);
    }

    // --- Delete a department (soft delete) ---
    @Transactional
    public void deleteDepartment(UUID departmentId, Boolean soft, Authentication authentication) {
        if(Boolean.TRUE.equals(soft)) {
            Department d = departmentRepository.findByIdAndDeletedFalse(departmentId)
                    .orElseThrow(() -> new EntityNotFoundException("Department not found"));
            d.setDeleted(true);
            departmentRepository.save(d);

            logAudit(d, "DELETE", mapDepartment(d).toString(), null, authentication, "Soft delete");


        } else {
            userDepartmentRepository.deleteAll(
                    userDepartmentRepository.findByDepartmentId(departmentId)
            );

            Department d = departmentRepository.findById(departmentId)
                    .orElseThrow(() -> new EntityNotFoundException("Department not found"));
            departmentRepository.delete(d);

            logAudit(d, "DELETE", mapDepartment(d).toString(), null, authentication, "Hard delete");
        }
    }

    @Transactional
    public void restoreDepartment(UUID departmentId, Authentication authentication) {
        Department d = departmentRepository.findByIdAndDeletedTrue(departmentId)
                .orElseThrow(() -> new EntityNotFoundException("No such deleted department found"));
        d.setDeleted(false);
        departmentRepository.save(d);

        logAudit(d, "RESTORE", mapDepartment(d).toString(), null, authentication, "Department restore");
    }





    // HELPER METHODS

    private Set<User> fetchUsers(Set<MinimalUserDTO> dtos) {
        return dtos.stream()
                .map(u -> userRepository.findByIdAndDeletedFalse(u.getId())
                        .orElseThrow(() -> new IllegalArgumentException("User not found: " + (u.getId()))))
                .peek(u -> {
                    // If user is not allowed to be head based on current role, promote to SUPERVISOR
                    if (!privilegesChecker.isAuthorizedToBeHead(u)) {
                        u.setRole(Role.SUPERVISOR);
                        userRepository.save(u); // Persist the promotion
                    }
                })
                .collect(Collectors.toSet());
    }

    private Set<User> resolveUsers(Set<UUID> ids, boolean asHead) {
        if (ids == null || ids.isEmpty()) return Set.of();

        return ids.stream()
                .map(id -> userRepository.findByIdAndDeletedFalse(id)
                        .orElseThrow(() ->
                                new IllegalArgumentException("User not found: " + id)))
                .peek(user -> {
                    if (asHead && !privilegesChecker.isAuthorizedToBeHead(user)) {
                        user.setRole(Role.SUPERVISOR);
                        userRepository.save(user);
                    }
                })
                .collect(Collectors.toSet());
    }

    private void checkOverlap(Set<User> heads, Set<User> members) {
        Set<User> overlap = heads.stream().filter(members::contains).collect(Collectors.toSet());
        if (!overlap.isEmpty()) {
            String conflicts = overlap.stream().map(User::getUsername).collect(Collectors.joining(", "));
            throw new IllegalArgumentException("Users cannot be both heads and members: " + conflicts);
        }
    }

    private Map<String, String> mapDepartment(Department department) {

        Map<String, String> map = new HashMap<>();

        map.put("name", department.getName());
        map.put("description", department.getDescription());
        map.put("rollcallStartTime", String.valueOf(department.getRollcallStartTime()));
        map.put("gracePeriodMinutes", String.valueOf(department.getGracePeriodMinutes()));

        List<UserDepartment> relations =
                userDepartmentRepository.findByDepartmentId(department.getId());

        String heads = relations.stream()
                .filter(r -> r.getRole() == DepartmentMembershipRole.HEAD)
                .map(r -> r.getUser().getUsername())
                .collect(Collectors.joining(", "));

        String members = relations.stream()
                .filter(r -> r.getRole() == DepartmentMembershipRole.MEMBER)
                .map(r -> r.getUser().getUsername())
                .collect(Collectors.joining(", "));

        map.put("heads", heads);
        map.put("members", members);

        return map;
    }

    public void logAudit(Department department, String action, String oldValue, String newValue, Authentication authentication, String reason) {
        User actor = privilegesChecker.getAuthenticatedUser(authentication);

        DepartmentAudit audit = DepartmentAudit.builder()
                .departmentId(department.getId())
                .departmentName(department.getName())
                .action(action)
                .fieldChanged(null) // optional: can be detailed per field
                .oldValue(oldValue)
                .newValue(newValue)
                .reason(reason)
                .performedById(actor.getId())
                .performedByUsername(actor.getUsername())
                .build();

        departmentAuditRepository.save(audit);
    }
}