package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.ParamDef;

import java.util.UUID;

@MappedSuperclass
@FilterDef(
        name = "branchFilter",
        parameters = @ParamDef(name = "branchId", type = UUID.class)
)
@Filter(
        name = "branchFilter",
        condition = "branch_id = :branchId"
)
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class BranchAwareEntity extends TenantAwareEntity {
    @Column(name = "branch_id")
    private UUID branchId;
}