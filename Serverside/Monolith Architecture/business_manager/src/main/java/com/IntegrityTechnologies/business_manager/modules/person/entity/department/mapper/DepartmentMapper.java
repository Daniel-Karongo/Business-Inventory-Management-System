package com.IntegrityTechnologies.business_manager.modules.person.entity.department.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class DepartmentMapper {

    private final DepartmentRepository departmentRepository;

    public DepartmentDTO toDTO(Department d) {
        DepartmentDTO dto = new DepartmentDTO();
        dto.setId(d.getId());
        dto.setName(d.getName());
        dto.setDescription(d.getDescription());
        dto.setRollcallStartTime(d.getRollcallStartTime());
        dto.setGracePeriodMinutes(d.getGracePeriodMinutes());

        dto.setHeads(d.getHeads().stream()
                .map(u -> new MinimalUserDTO(u.getId(), u.getUsername()))
                .collect(Collectors.toSet()));

        dto.setMembers(d.getMembers().stream()
                .map(u -> new MinimalUserDTO(u.getId(), u.getUsername()))
                .collect(Collectors.toSet()));

        // fetch branches for this department
        List<Branch> branches = departmentRepository.findBranchesByDepartmentId(d.getId());

        dto.setBranches(
                branches.stream()
                        .map(BranchMinimalDTO::from)
                        .collect(Collectors.toSet())
        );

        return dto;
    }
}