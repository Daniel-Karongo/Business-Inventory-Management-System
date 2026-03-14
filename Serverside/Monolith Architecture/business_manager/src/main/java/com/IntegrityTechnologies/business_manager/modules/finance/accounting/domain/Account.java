package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(
        name = "accounts",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_account_tenant_branch_code",
                columnNames = {"tenant_id","branch_id","code"}
        ),
        indexes = {
                @Index(
                        name = "idx_account_tenant_branch_code",
                        columnList = "tenant_id,branch_id,code"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class Account extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private String code;

    @Column(nullable = false)
    private String name;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private AccountRole role;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private AccountType type;

    @Column(nullable = false)
    private boolean active = true;

    @Version
    private Long version;

    public Account(
            UUID tenantId,
            UUID branchId,
            String code,
            String name,
            AccountType type,
            AccountRole role
    ) {
        this.setTenantId(tenantId);
        this.setBranchId(branchId);
        this.code = code;
        this.name = name;
        this.type = type;
        this.role = role;
    }
}