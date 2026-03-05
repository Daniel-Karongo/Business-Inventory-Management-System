package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

@Entity
@Table(
        name = "ledger_entries",
        indexes = {

                @Index(
                        name = "idx_ledger_branch_account_posted",
                        columnList = "branch_id, account_id, postedAt"
                ),

                @Index(
                        name = "idx_ledger_account_posted_direction",
                        columnList = "account_id, postedAt, direction"
                ),

                @Index(
                        name = "idx_ledger_journal_account",
                        columnList = "journal_entry_id, account_id"
                ),

                @Index(
                        name = "idx_ledger_postedAt",
                        columnList = "postedAt"
                ),

                @Index(
                        name = "idx_ledger_account_only",
                        columnList = "account_id"
                ),
                @Index(
                        name = "idx_ledger_branch_postedAt_id",
                        columnList = "branch_id, postedAt, id"
                )
        }
)
@Getter
@NoArgsConstructor
@IdClass(LedgerEntry.LedgerEntryId.class)
public class LedgerEntry {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Id
    @Column(name = "branch_id", nullable = false)
    private UUID branchId;

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

    @Column(nullable = false)
    private LocalDateTime postedAt;

    public LedgerEntry(Account account,
                       JournalEntry journalEntry,
                       EntryDirection direction,
                       BigDecimal amount) {

        this.account = account;
        this.journalEntry = journalEntry;
        this.direction = direction;
        this.amount = amount;
        this.postedAt = journalEntry.getPostedAt();

        this.branchId = journalEntry.getBranch().getId();
    }

    @PreUpdate
    public void preventUpdate() {
        throw new IllegalStateException("Ledger entries are immutable and cannot be updated.");
    }

    @PreRemove
    public void preventDelete() {
        throw new IllegalStateException("Ledger entries cannot be deleted.");
    }

    public static class LedgerEntryId implements Serializable {

        private UUID id;
        private UUID branchId;

        public LedgerEntryId() {}

        public LedgerEntryId(UUID id, UUID branchId) {
            this.id = id;
            this.branchId = branchId;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof LedgerEntryId that)) return false;
            return Objects.equals(id, that.id) &&
                    Objects.equals(branchId, that.branchId);
        }

        @Override
        public int hashCode() {
            return Objects.hash(id, branchId);
        }
    }
}