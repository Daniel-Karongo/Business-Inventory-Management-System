package com.IntegrityTechnologies.business_manager.modules.finance.accounting.api;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import lombok.Builder;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Getter
@Builder
public class AccountingEvent {

    private String sourceModule;
    private UUID sourceId;
    private String reference;
    private String description;
    private String performedBy;

    private List<Entry> entries;

    @Getter
    @Builder
    public static class Entry {
        private UUID accountId;
        private EntryDirection direction;
        private BigDecimal amount;
    }
}