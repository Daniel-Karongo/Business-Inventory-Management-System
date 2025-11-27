package com.IntegrityTechnologies.business_manager.modules.finance.accounts.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(name = "journal_entry_lines")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EntryLine {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "journal_entry_id")
    private JournalEntry journalEntry;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id")
    private Account account;

    @Column(precision = 19, scale = 2)
    private BigDecimal debit;

    @Column(precision = 19, scale = 2)
    private BigDecimal credit;

    private String note;

    // convenience (denormalized)
    private String transactionType; // SALE, PURCHASE, PAYMENT, ADJUSTMENT
}