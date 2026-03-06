package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

@Entity
@Table(
        name = "accounts",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_account_branch_code",
                columnNames = {"branch_id", "code"}
        ),
        indexes = {
                @Index(name = "idx_account_branch_code", columnList = "branch_id, code")
        }
)
@Getter
@NoArgsConstructor
@Setter
public class Account {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(name = "branch_id", nullable = false)
    private UUID branchId;

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
            UUID branchId,
            String code,
            String name,
            AccountType type,
            AccountRole role
    ) {
        this.branchId = branchId;
        this.code = code;
        this.name = name;
        this.type = type;
        this.role = role;
    }
}