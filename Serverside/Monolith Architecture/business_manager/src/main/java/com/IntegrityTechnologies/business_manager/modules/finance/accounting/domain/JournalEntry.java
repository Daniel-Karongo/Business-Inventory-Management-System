package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "journal_entries")
@Getter
@NoArgsConstructor
@Setter
public class JournalEntry {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String reference;
    private String sourceModule;
    private UUID sourceId;
    private String description;

    private String postedBy;
    private LocalDateTime postedAt;
    private boolean posted;

    private boolean reversed;
    @Column(unique = true)
    private UUID reversalJournalId;

    @OneToMany(mappedBy = "journalEntry", cascade = CascadeType.ALL)
    private List<LedgerEntry> ledgerEntries = new ArrayList<>();

    /* ============================================================
       STATE TRANSITIONS
    ============================================================ */

    public void markPosted(String user) {
        if (this.posted) {
            throw new IllegalStateException("Journal already posted");
        }
        this.posted = true;
        this.postedBy = user;
        this.postedAt = LocalDateTime.now();
    }

    public void markReversed(UUID reversalId) {
        if (!this.posted) {
            throw new IllegalStateException("Cannot reverse an unposted journal");
        }
        if (this.reversed) {
            throw new IllegalStateException("Journal already reversed");
        }
        this.reversed = true;
        this.reversalJournalId = reversalId;
    }
}