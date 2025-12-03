package com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto;

import lombok.Data;

import java.util.List;

@Data
public class JournalEntryRequest {
    private String reference;
    private String description;
    private String transactionCode;
    private List<EntryLineRequest> lines;
}