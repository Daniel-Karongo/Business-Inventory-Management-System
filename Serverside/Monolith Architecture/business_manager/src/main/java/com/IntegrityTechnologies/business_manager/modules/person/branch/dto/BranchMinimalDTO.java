package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import lombok.*;

import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BranchMinimalDTO {
    private UUID id;
    private String branchCode;
    private String name;

    public static BranchMinimalDTO from(Branch b) {
        BranchMinimalDTO dto = new BranchMinimalDTO();
        dto.setId(b.getId());
        dto.setName(b.getName());
        dto.setBranchCode(b.getBranchCode());
        return dto;
    }
}