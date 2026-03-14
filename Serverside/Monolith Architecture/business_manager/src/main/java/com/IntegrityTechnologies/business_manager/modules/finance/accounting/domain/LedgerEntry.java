package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "ledger_entries",
        indexes = {

                @Index(name = "idx_ledger_tenant_branch_account_posted",
                        columnList = "tenant_id,branch_id,account_id,postedAt"),

                @Index(name = "idx_ledger_account_posted_direction",
                        columnList = "account_id,postedAt,direction"),

                @Index(name = "idx_ledger_journal_account",
                        columnList = "journal_entry_id,account_id"),

                @Index(name = "idx_ledger_postedAt",
                        columnList = "postedAt"),

                @Index(name = "idx_ledger_account_only",
                        columnList = "account_id"),

                @Index(name = "idx_ledger_branch_postedAt_id",
                        columnList = "branch_id,postedAt,id")
        }
)
@Getter
@NoArgsConstructor
public class LedgerEntry extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "journal_entry_id", nullable = false)
    private JournalEntry journalEntry;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private EntryDirection direction;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(name = "posted_at", nullable = false)
    private LocalDateTime postedAt;

    public LedgerEntry(
            UUID tenantId,
            UUID branchId,
            Account account,
            JournalEntry journalEntry,
            EntryDirection direction,
            BigDecimal amount
    ) {

        if (!account.getTenantId().equals(tenantId))
            throw new IllegalStateException("Account tenant mismatch");

        if (!account.getBranchId().equals(branchId))
            throw new IllegalStateException("Account branch mismatch");

        this.setTenantId(tenantId);
        this.setBranchId(branchId);

        this.account = account;
        this.journalEntry = journalEntry;
        this.direction = direction;
        this.amount = amount;

        this.postedAt = journalEntry.getPostedAt();
    }

    @PreUpdate
    public void preventUpdate() {
        throw new IllegalStateException("Ledger entries are immutable and cannot be updated.");
    }

    @PreRemove
    public void preventDelete() {
        throw new IllegalStateException("Ledger entries cannot be deleted.");
    }
}