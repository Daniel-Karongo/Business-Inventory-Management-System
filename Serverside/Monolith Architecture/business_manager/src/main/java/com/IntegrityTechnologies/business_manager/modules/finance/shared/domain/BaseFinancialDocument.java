package com.IntegrityTechnologies.business_manager.modules.finance.shared.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.Column;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.MappedSuperclass;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@MappedSuperclass
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseFinancialDocument
        extends BranchAwareEntity {

    @Column(nullable = false, updatable = false)
    protected String documentNumber;

    @Column(nullable = false)
    protected LocalDate documentDate;

    @Column(nullable = false)
    protected LocalDate postingDate;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    protected FinancialDocumentStatus documentStatus;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    protected FinancialPostingStatus postingStatus;

    @Column(columnDefinition = "BINARY(16)")
    protected UUID journalEntryId;

    @Column(columnDefinition = "BINARY(16)")
    protected UUID reversalJournalEntryId;

    @Column(nullable = false)
    protected boolean reversed = false;

    protected LocalDateTime reversedAt;

    protected String reversedBy;

    protected String reversalReason;

    @Column(nullable = false)
    protected boolean posted = false;

    protected LocalDateTime postedAt;

    protected String postedBy;
}