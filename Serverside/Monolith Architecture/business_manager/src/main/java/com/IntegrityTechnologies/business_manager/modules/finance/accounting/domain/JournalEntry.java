package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import jakarta.persistence.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "journal_entries",
        indexes = {

                @Index(
                        name = "idx_journal_branch_status_date",
                        columnList = "branch_id, posted, reversed, accountingDate"
                ),

                @Index(
                        name = "idx_journal_branch_postedAt",
                        columnList = "branch_id, postedAt"
                ),

                @Index(
                        name = "idx_journal_period",
                        columnList = "periodId"
                ),

                @Index(
                        name = "idx_journal_accountingDate",
                        columnList = "accountingDate"
                ),

                @Index(
                        name = "idx_journal_branch_postedAt_hash",
                        columnList = "branch_id, postedAt, hash"
                )
        },
        uniqueConstraints = {

                @UniqueConstraint(
                        name = "uk_journal_source",
                        columnNames = {"sourceModule", "sourceId"}
                ),

                @UniqueConstraint(
                        name = "uk_journal_event_id",
                        columnNames = {"accountingEventId"}
                )
        }
)
public class JournalEntry {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, updatable = false)
    private String sourceModule;

    @Column(nullable = false, updatable = false)
    private UUID sourceId;

    @Column(nullable = false, updatable = false)
    private String reference;

    @Column(updatable = false)
    private String description;

    @Column(nullable = false, updatable = false)
    private String postedBy;

    @Column(nullable = false, updatable = false)
    private LocalDateTime postedAt;

    @Column(nullable = false, updatable = false)
    private LocalDate accountingDate;

    @Column(nullable = false, updatable = false)
    private UUID periodId;
    @Column(nullable = false, unique = true, updatable = false)
    private UUID accountingEventId;

    @Column(nullable = false)
    private boolean posted = false;

    @Column(nullable = false)
    private boolean reversed = false;

    @Column(unique = true)
    private UUID reversalJournalId;

    @OneToMany(mappedBy = "journalEntry", cascade = CascadeType.ALL, orphanRemoval = false)
    private List<LedgerEntry> ledgerEntries = new ArrayList<>();

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "branch_id", nullable = false)
    private Branch branch;

    @Column(length = 64, updatable = false)
    private String previousHash;

    @Column(length = 64, unique = true)
    private String hash;
    protected JournalEntry() {}

    public JournalEntry(
            String reference,
            String sourceModule,
            UUID sourceId,
            String description,
            Branch branch,
            LocalDate accountingDate,
            UUID accountingEventId,
            AccountingPeriod period
    ) {

        this.reference = reference;
        this.sourceModule = sourceModule;
        this.sourceId = sourceId;
        this.description = description;
        this.branch = branch;
        this.accountingDate = accountingDate;
        this.accountingEventId = accountingEventId;
        this.periodId = period.getId();
    }

    public UUID getId() { return id; }
    public String getReference() { return reference; }
    public String getSourceModule() { return sourceModule; }
    public UUID getSourceId() { return sourceId; }
    public String getDescription() { return description; }
    public String getPostedBy() { return postedBy; }
    public LocalDateTime getPostedAt() { return postedAt; }
    public LocalDate getAccountingDate() { return accountingDate; }
    public UUID getAccountingEventId() { return accountingEventId; }
    public boolean isPosted() { return posted; }
    public boolean isReversed() { return reversed; }
    public UUID getReversalJournalId() { return reversalJournalId; }
    public Branch getBranch() { return branch; }
    public List<LedgerEntry> getLedgerEntries() { return ledgerEntries; }
    public String getHash() { return hash; }
    public String getPreviousHash() { return previousHash; }

    public void setPreviousHash(String previousHash) {
        this.previousHash = previousHash;
    }

    public void setHash(String hash) {
        this.hash = hash;
    }

    public void markPosted(String user) {

        if (this.posted)
            throw new IllegalStateException("Journal already posted");

        if (user == null)
            throw new IllegalStateException("PostedBy user required");

        this.posted = true;
        this.postedBy = user;
        this.postedAt = LocalDateTime.now();
    }

    public void markReversed(UUID reversalId) {

        if (!this.posted)
            throw new IllegalStateException("Cannot reverse unposted journal");

        if (this.reversed)
            throw new IllegalStateException("Journal already reversed");

        this.reversed = true;
        this.reversalJournalId = reversalId;
    }

    @PreUpdate
    public void preventModification() {

        if (posted)
            throw new IllegalStateException("Posted journal entries are immutable");
    }
}