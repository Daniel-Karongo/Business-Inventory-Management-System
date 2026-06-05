package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;
import java.util.UUID;

@Entity
@Table(
        name = "account_balances",
        indexes = {
                @Index(
                        name = "idx_balance_tenant_branch_account",
                        columnList = "tenant_id, branch_id, account_id"
                ),
                @Index(
                        name = "idx_balance_updated",
                        columnList = "updated_at"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@IdClass(AccountBalance.AccountBalanceId.class)
public class AccountBalance extends TenantAwareEntity {

    @Id
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(
            name = "account_id",
            nullable = false
    )
    private Account account;

    @Id
    @Column(name = "branch_id", nullable = false)
    private UUID branchId;

    @Column(
            nullable = false,
            precision = 19,
            scale = 2
    )
    private BigDecimal balance = BigDecimal.ZERO;

    public static class AccountBalanceId
            implements Serializable {

        private UUID account;
        private UUID branchId;

        public AccountBalanceId() {
        }

        public AccountBalanceId(
                UUID account,
                UUID branchId
        ) {
            this.account = account;
            this.branchId = branchId;
        }

        @Override
        public boolean equals(
                Object o
        ) {
            if (this == o) {
                return true;
            }

            if (!(o instanceof AccountBalanceId that)) {
                return false;
            }

            return Objects.equals(
                    account,
                    that.account
            ) &&
                    Objects.equals(
                            branchId,
                            that.branchId
                    );
        }

        @Override
        public int hashCode() {
            return Objects.hash(
                    account,
                    branchId
            );
        }
    }
}