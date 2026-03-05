package com.IntegrityTechnologies.business_manager.modules.finance.accounting.security;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Comparator;

public class JournalHashUtil {

    public static String sha256(String input) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(input.getBytes(StandardCharsets.UTF_8));

            StringBuilder hex = new StringBuilder();
            for (byte b : hash) {
                hex.append(String.format("%02x", b));
            }
            return hex.toString();

        } catch (Exception e) {
            throw new RuntimeException("Hashing failed", e);
        }
    }

    public static String computeJournalHash(JournalEntry journal, String previousHash) {

        StringBuilder data = new StringBuilder();

        data.append(previousHash == null ? "" : previousHash);
        data.append(journal.getSourceModule());
        data.append(journal.getSourceId());
        data.append(journal.getReference());
        data.append(journal.getAccountingDate());
        data.append(journal.getBranch().getId());

        journal.getLedgerEntries()
                .stream()
                .sorted(Comparator.comparing(l -> l.getAccount().getId()))
                .forEach(le -> {
                    data.append(le.getAccount().getId());
                    data.append(le.getDirection());
                    data.append(le.getAmount());
                });

        return sha256(data.toString());
    }
}