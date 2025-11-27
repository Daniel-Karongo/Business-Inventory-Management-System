package com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class JournalEntryResponse {
    private UUID id;
    private String reference;
    private String description;
    private LocalDateTime timestamp;
    private String createdBy;
    private List<EntryLineResponse> lines;

    @Data
    @Builder
    public static class EntryLineResponse {
        private UUID id;
        private UUID accountId;
        private String accountName;
        private java.math.BigDecimal debit;
        private java.math.BigDecimal credit;
        private String note;
    }
}