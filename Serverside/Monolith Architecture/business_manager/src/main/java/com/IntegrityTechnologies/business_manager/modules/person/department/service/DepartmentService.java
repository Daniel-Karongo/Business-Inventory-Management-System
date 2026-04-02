package com.IntegrityTechnologies.business_manager.modules.person.department.service;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.mapper.DepartmentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserDepartment;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserDepartmentId;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
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
    private final BranchTenantGuard branchTenantGuard;
    private final DepartmentMapper departmentMapper;
    private final UserDepartmentRepository userDepartmentRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    
    @Transactional
    public DepartmentDTO create(DepartmentDTO dto, Authentication authentication) {

        
        branchTenantGuard.validate(dto.getBranchId());

        if (departmentRepository.existsByTenantIdAndNameIgnoreCaseAndBranch_Id(
                tenantId(),
                dto.getName(),
                dto.getBranchId()
        )) {
            throw new IllegalArgumentException(
                    "Department with name '" + dto.getName() + "' already exists in this branch"
            );
        }

        Branch branch = branchRepository
                .findByTenantIdAndId(tenantId(), dto.getBranchId())
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

            

            User user = userRepository
                    .findByIdAndTenantIdAndDeletedFalse(userId, tenantId())
                    .filter(u -> u.getTenantId().equals(tenantId()))
                    .orElseThrow(() -> new IllegalArgumentException("User not found: " + userId));

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

        
        branchTenantGuard.validate(department.getBranch().getId());

        if (!department.getName().equalsIgnoreCase(dto.getName()) &&
                departmentRepository.existsByTenantIdAndNameIgnoreCaseAndBranch_Id(
                        tenantId(),
                        dto.getName(),
                        department.getBranch().getId()
                )) {

            throw new IllegalArgumentException(
                    "Department with the name '" + dto.getName() + "' already exists"
            );
        }

        Map<String, String> oldValues = mapDepartment(department);

        department.setName(dto.getName());
        department.setDescription(dto.getDescription());
        department.setRollcallStartTime(dto.getRollcallStartTime());
        department.setGracePeriodMinutes(dto.getGracePeriodMinutes());

        departmentRepository.save(department);

        // 🔹 Remove old user relations
        userDepartmentRepository.deleteByDepartmentId(tenantId(), department.getId());

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

        userDepartmentRepository.deleteByDepartmentId(tenantId(), existing.getId());

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

        

        Set<Department> departments =
                departmentRepository.findDepartmentsByUserId(tenantId(), userId);

        return departments.stream()
                .map(DepartmentMinimalDTO::from)
                .toList();
    }

    public Department getById(UUID id) {

        

        return departmentRepository
                .findByTenantIdAndId(tenantId(), id)
                .orElseThrow(() -> new EntityNotFoundException("Department not found"));
    }

    // --- Get all active departments ---
    public List<DepartmentDTO> getAllDepartments(Boolean deleted) {

        

        List<Department> departments;

        if (deleted == null) {
            departments = departmentRepository.findByTenantIdAndDeletedFalse(tenantId());
        } else if (deleted) {
            departments = departmentRepository.findByTenantIdAndDeletedTrue(tenantId());
        } else {
            departments = departmentRepository.findByTenantIdAndDeletedFalse(tenantId());
        }

        return departments.stream()
                .map(departmentMapper::toDTO)
                .toList();
    }

    public List<DepartmentAudit> getAllDepartmentsAudits() {

        
        UUID branchId = BranchContext.getOrNull();

        if (branchId != null) {
            return departmentAuditRepository
                    .findByTenantIdAndBranchIdOrderByTimestampDesc(
                            tenantId(),
                            branchId
                    );
        }

        return departmentAuditRepository
                .findByTenantIdOrderByTimestampDesc(tenantId());
    }

    public List<DepartmentAudit> getDepartmentAudits(UUID departmentId) {

        
        UUID branchId = BranchContext.getOrNull();

        if (branchId != null) {
            return departmentAuditRepository
                    .findByTenantIdAndBranchIdAndDepartmentIdOrderByTimestampDesc(
                            tenantId(),
                            branchId,
                            departmentId
                    );
        }

        return departmentAuditRepository
                .findByTenantIdAndDepartmentIdOrderByTimestampDesc(
                        tenantId(),
                        departmentId
                );
    }

    public List<DepartmentAudit> getDepartmentAuditsByPerformer(UUID userId) {

        
        UUID branchId = BranchContext.getOrNull();

        if (branchId != null) {
            return departmentAuditRepository
                    .findByTenantIdAndBranchIdAndPerformedByIdOrderByTimestampDesc(
                            tenantId(),
                            branchId,
                            userId
                    );
        }

        return departmentAuditRepository
                .findByTenantIdAndPerformedByIdOrderByTimestampDesc(
                        tenantId(),
                        userId
                );
    }

    // --- Delete a department (soft delete) ---
    @Transactional
    public void deleteDepartment(UUID departmentId, Boolean soft, Authentication authentication) {

        

        if (Boolean.TRUE.equals(soft)) {

            Department d = departmentRepository
                    .findByTenantIdAndIdAndDeletedFalse(tenantId(), departmentId)
                    .orElseThrow(() -> new EntityNotFoundException("Department not found"));

            branchTenantGuard.validate(d.getBranch().getId());

            d.setDeleted(true);

            departmentRepository.save(d);

            logAudit(d, "DELETE", mapDepartment(d).toString(), null, authentication, "Soft delete");

        } else {

            Department d = departmentRepository
                    .findByTenantIdAndId(tenantId(), departmentId)
                    .orElseThrow(() -> new EntityNotFoundException("Department not found"));

            branchTenantGuard.validate(d.getBranch().getId());

            userDepartmentRepository.deleteByDepartmentId(tenantId(), departmentId);

            departmentRepository.delete(d);

            logAudit(d, "DELETE", mapDepartment(d).toString(), null, authentication, "Hard delete");
        }
    }

    @Transactional
    public void restoreDepartment(UUID departmentId, Authentication authentication) {

        

        Department d = departmentRepository
                .findByTenantIdAndIdAndDeletedTrue(tenantId(), departmentId)
                .orElseThrow(() -> new EntityNotFoundException("No such deleted department found"));
        branchTenantGuard.validate(d.getBranch().getId());

        d.setDeleted(false);

        departmentRepository.save(d);

        logAudit(d, "RESTORE", mapDepartment(d).toString(), null, authentication, "Department restore");
    }

    // HELPER METHODS

    private Map<String, String> mapDepartment(Department department) {

        Map<String, String> map = new HashMap<>();

        map.put("name", department.getName());
        map.put("description", department.getDescription());
        map.put("rollcallStartTime", String.valueOf(department.getRollcallStartTime()));
        map.put("gracePeriodMinutes", String.valueOf(department.getGracePeriodMinutes()));

        List<UserDepartment> relations =
                userDepartmentRepository
                        .findByDepartmentIdWithUser(
                                TenantContext.getTenantId(),
                                department.getId()
                        );

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