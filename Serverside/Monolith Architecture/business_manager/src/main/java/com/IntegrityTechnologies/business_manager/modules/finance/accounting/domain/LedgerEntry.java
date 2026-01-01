package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
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
                @Index(columnList = "account_id"),
                @Index(columnList = "journal_entry_id")
        }
)
@Getter
@NoArgsConstructor
public class LedgerEntry {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    private Account account;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
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
        this.postedAt = LocalDateTime.now();
    }
}