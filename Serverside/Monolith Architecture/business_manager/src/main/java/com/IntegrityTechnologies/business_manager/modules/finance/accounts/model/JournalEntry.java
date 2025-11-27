package com.IntegrityTechnologies.business_manager.modules.finance.accounts.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "journal_entries", indexes = {
        @Index(name = "idx_journal_ref", columnList = "reference")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class JournalEntry {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String reference; // e.g., SALE-2025-0001

    private String description;

    private LocalDateTime timestamp;

    private String createdBy;

    @OneToMany(mappedBy = "journalEntry", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<EntryLine> lines;
}