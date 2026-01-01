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

    @OneToMany(mappedBy = "journalEntry", cascade = CascadeType.ALL)
    private List<LedgerEntry> ledgerEntries = new ArrayList<>();

    public void markPosted(String user) {
        this.posted = true;
        this.postedBy = user;
        this.postedAt = LocalDateTime.now();
    }
}