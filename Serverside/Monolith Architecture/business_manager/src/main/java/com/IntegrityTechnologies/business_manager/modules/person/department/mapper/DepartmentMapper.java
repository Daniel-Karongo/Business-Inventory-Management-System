package com.IntegrityTechnologies.business_manager.modules.person.department.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserDepartment;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class DepartmentMapper {

    private final UserDepartmentRepository userDepartmentRepository;

    public DepartmentDTO toDTO(Department d) {

        DepartmentDTO dto = new DepartmentDTO();

        dto.setId(d.getId());
        dto.setName(d.getName());
        dto.setDescription(d.getDescription());
        dto.setRollcallStartTime(d.getRollcallStartTime());
        dto.setGracePeriodMinutes(d.getGracePeriodMinutes());

        // 🔹 Branch (single)
        if (d.getBranch() != null) {
            dto.setBranch(
                    new BranchMinimalDTO(
                            d.getBranch().getId(),
                            d.getBranch().getBranchCode(),
                            d.getBranch().getName()
                    )
            );
        }

        // 🔹 Membership from UserDepartment
        UUID tenantId = TenantContext.getTenantId();

        List<UserDepartment> relations =
                userDepartmentRepository
                        .findByDepartmentIdWithUser(tenantId, d.getId());

        dto.setHeads(
                relations.stream()
                        .filter(r -> r.getRole() == DepartmentMembershipRole.HEAD)
                        .map(r -> new MinimalUserDTO(
                                r.getUser().getId(),
                                r.getUser().getUsername()
                        ))
                        .collect(Collectors.toSet())
        );

        dto.setMembers(
                relations.stream()
                        .filter(r -> r.getRole() == DepartmentMembershipRole.MEMBER)
                        .map(r -> new MinimalUserDTO(
                                r.getUser().getId(),
                                r.getUser().getUsername()
                        ))
                        .collect(Collectors.toSet())
        );

        return dto;
    }
}