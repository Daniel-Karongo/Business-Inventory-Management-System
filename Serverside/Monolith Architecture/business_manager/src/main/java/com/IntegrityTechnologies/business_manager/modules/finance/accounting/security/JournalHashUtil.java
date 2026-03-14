package com.IntegrityTechnologies.business_manager.modules.finance.accounting.security;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Comparator;

public class JournalHashUtil {

    private static final String SEP = "|";

    public static String sha256(String input) {

        try {

            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            byte[] hash = digest.digest(
                    input.getBytes(StandardCharsets.UTF_8)
            );

            StringBuilder hex = new StringBuilder();

            for (byte b : hash) {
                hex.append(String.format("%02x", b));
            }

            return hex.toString();

        } catch (Exception e) {
            throw new RuntimeException("Hashing failed", e);
        }
    }

    public static String computeJournalHash(
            JournalEntry journal,
            String previousHash
    ) {

        StringBuilder data = new StringBuilder();

        /* =====================================================
           CHAIN LINK
        ===================================================== */

        data.append(previousHash == null ? "GENESIS" : previousHash)
                .append(SEP);

        /* =====================================================
           JOURNAL METADATA
        ===================================================== */

        data.append(journal.getTenantId()).append(SEP);
        data.append(journal.getBranch().getId()).append(SEP);
        data.append(journal.getSourceModule()).append(SEP);
        data.append(journal.getSourceId()).append(SEP);
        data.append(journal.getReference()).append(SEP);
        data.append(journal.getAccountingDate()).append(SEP);

        /* =====================================================
           LEDGER ENTRIES
        ===================================================== */

        journal.getLedgerEntries()
                .stream()
                .sorted(
                        Comparator
                                .comparing((LedgerEntry l) -> l.getAccount().getId())
                                .thenComparing(LedgerEntry::getDirection)
                                .thenComparing(LedgerEntry::getAmount)
                )
                .forEach(le -> {

                    data.append(le.getAccount().getId()).append(SEP);
                    data.append(le.getDirection()).append(SEP);
                    data.append(le.getAmount()).append(SEP);

                });

        return sha256(data.toString());
    }
}