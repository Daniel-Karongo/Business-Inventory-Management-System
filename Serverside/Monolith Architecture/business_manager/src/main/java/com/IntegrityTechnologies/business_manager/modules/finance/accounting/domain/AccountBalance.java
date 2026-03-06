package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

@Entity
@Table(
        name = "account_balances",
        indexes = {
                @Index(name = "idx_balance_branch_account", columnList = "branch_id, account_id"),
                @Index(name = "idx_balance_updated", columnList = "updatedAt")
        }
)
@Getter
@Setter
@NoArgsConstructor
@IdClass(AccountBalance.AccountBalanceId.class)
public class AccountBalance {

    @Id
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Id
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "branch_id", nullable = false)
    private Branch branch;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal balance = BigDecimal.ZERO;

    @Column(nullable = false)
    private LocalDateTime updatedAt;

    @Version
    private Long version;

    public static class AccountBalanceId implements Serializable {
        private UUID account;
        private UUID branch;

        public AccountBalanceId() {}

        public AccountBalanceId(UUID account, UUID branch) {
            this.account = account;
            this.branch = branch;
        }

        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof AccountBalanceId that)) return false;
            return Objects.equals(account, that.account)
                    && Objects.equals(branch, that.branch);
        }

        public int hashCode() {
            return Objects.hash(account, branch);
        }
    }
}