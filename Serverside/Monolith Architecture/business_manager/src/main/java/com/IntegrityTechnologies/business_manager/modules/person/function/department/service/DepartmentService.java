package com.IntegrityTechnologies.business_manager.modules.person.function.department.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.dto.DepartmentUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
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

    @Transactional
    public DepartmentDTO create(DepartmentDTO dto, Authentication authentication) {
        if (departmentRepository.existsByNameIgnoreCase(dto.getName())) {
            throw new IllegalArgumentException("Department with the name '"+ dto.getName() +"' already exists");
        }

        // Fetch users
        Set<User> heads = dto.getHeads().stream()
                .map(head -> userRepository.findByIdAndDeletedFalse(head.getId())
                        .orElseThrow(() -> new IllegalArgumentException("Head user not found: " + head.getId())))
                .peek(user -> {
                    // If user is not allowed to be head based on current role, promote to SUPERVISOR
                    if (!privilegesChecker.isAuthorizedToBeHead(user)) {
                        user.setRole(Role.SUPERVISOR);
                        userRepository.save(user); // Persist the promotion
                    }
                })
                .collect(Collectors.toSet());


        Set<User> members = dto.getMembers().stream()
                .map(member -> userRepository.findByIdAndDeletedFalse(member.getId())
                        .orElseThrow(() -> new IllegalArgumentException("Member user not found: " + member.getId())))
                .collect(Collectors.toSet());

        // Check for overlap between heads and members
        Set<User> overlappingUsers = heads.stream()
                .filter(members::contains)
                .collect(Collectors.toSet());

        if (!overlappingUsers.isEmpty()) {
            String conflicts = overlappingUsers.stream()
                    .map(User::getUsername)
                    .collect(Collectors.joining(", "));
            throw new IllegalArgumentException("Users cannot be both heads and members: " + conflicts);
        }

        Department d = Department.builder()
                .name(dto.getName())
                .description(dto.getDescription())
                .heads(heads)
                .members(members)
                .rollcallStartTime(dto.getRollcallStartTime())
                .gracePeriodMinutes(dto.getGracePeriodMinutes())
                .build();

        Department department = departmentRepository.save(d);

        logAudit(department, "CREATE", null, dto.toString(), authentication, null);

        return DepartmentDTO.from(department);
    }


    @Transactional
    public DepartmentDTO updateDepartment(UUID departmentId, DepartmentDTO dto, Authentication authentication) {
        Department d = getById(departmentId);

        if (!d.getName().equalsIgnoreCase(dto.getName()) &&
                departmentRepository.existsByNameIgnoreCase(dto.getName())) {
            throw new IllegalArgumentException("Department with the name '" + dto.getName() + "' already exists");
        }

        Map<String, String> oldValues = mapDepartment(d);

        Set<User> newHeads = fetchUsers(dto.getHeads());
        Set<User> newMembers = fetchUsers(dto.getMembers());
        checkOverlap(newHeads, newMembers);

        // ------------------- AUDIT HEAD CHANGES -------------------
        Set<User> addedHeads = new HashSet<>(newHeads);
        addedHeads.removeAll(d.getHeads());

        Set<User> removedHeads = new HashSet<>(d.getHeads());
        removedHeads.removeAll(newHeads);

        for (User added : addedHeads) {
            logAudit(d, "ADD_HEAD", null, added.getUsername(), authentication, "Added head");
        }
        for (User removed : removedHeads) {
            logAudit(d, "REMOVE_HEAD", removed.getUsername(), null, authentication, "Removed head");
        }

        // ------------------- AUDIT MEMBER CHANGES -------------------
        Set<User> addedMembers = new HashSet<>(newMembers);
        addedMembers.removeAll(d.getMembers());

        Set<User> removedMembers = new HashSet<>(d.getMembers());
        removedMembers.removeAll(newMembers);

        for (User added : addedMembers) {
            logAudit(d, "ADD_MEMBER", null, added.getUsername(), authentication, "Added member");
        }
        for (User removed : removedMembers) {
            logAudit(d, "REMOVE_MEMBER", removed.getUsername(), null, authentication, "Removed member");
        }

        // ------------------- UPDATE OTHER FIELDS -------------------
        d.setName(dto.getName());
        d.setDescription(dto.getDescription());
        d.setRollcallStartTime(dto.getRollcallStartTime());
        d.setGracePeriodMinutes(dto.getGracePeriodMinutes());
        d.setHeads(newHeads);
        d.setMembers(newMembers);

        Department updated = departmentRepository.save(d);

        // Optional: audit general department info change
        Map<String, String> newValues = mapDepartment(updated);
        if (!oldValues.equals(newValues)) {
            logAudit(updated, "UPDATE", oldValues.toString(), newValues.toString(), authentication, "Updated department fields");
        }

        return DepartmentDTO.from(updated);
    }




    public List<DepartmentMinimalDTO> getAllDepartmentsForUser(UUID userId) {
        List<Department> departments = departmentRepository.findDepartmentsByUserId(userId);
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
                .map(DepartmentDTO::from)
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

    private Set<User> fetchUsers(Set<DepartmentUserDTO> dtos) {
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

    private void checkOverlap(Set<User> heads, Set<User> members) {
        Set<User> overlap = heads.stream().filter(members::contains).collect(Collectors.toSet());
        if (!overlap.isEmpty()) {
            String conflicts = overlap.stream().map(User::getUsername).collect(Collectors.joining(", "));
            throw new IllegalArgumentException("Users cannot be both heads and members: " + conflicts);
        }
    }

    private Map<String, String> mapDepartment(Department d) {
        Map<String, String> map = new HashMap<>();
        map.put("name", d.getName());
        map.put("description", d.getDescription());
        map.put("rollcallStartTime", String.valueOf(d.getRollcallStartTime()));
        map.put("gracePeriodMinutes", String.valueOf(d.getGracePeriodMinutes()));
        map.put("heads", d.getHeads().stream().map(User::getUsername).collect(Collectors.joining(", ")));
        map.put("members", d.getMembers().stream().map(User::getUsername).collect(Collectors.joining(", ")));
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