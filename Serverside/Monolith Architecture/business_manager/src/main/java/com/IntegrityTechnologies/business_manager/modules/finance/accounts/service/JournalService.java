package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryResponse;

import java.util.List;
import java.util.UUID;

public interface JournalService {
    JournalEntryResponse createJournalEntry(JournalEntryRequest req, String performedBy);
    JournalEntryResponse getJournalEntry(UUID id);
    List<JournalEntryResponse> listJournalEntries();
}