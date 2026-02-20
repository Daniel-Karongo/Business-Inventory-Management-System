package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.AccountingPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class LedgerPostingService {

    private final AccountingPolicy policy;
    private final JournalEntryRepository journalRepo;
    private final AccountBalanceRepository balanceRepo;

    @Transactional
    public JournalEntry post(JournalEntry journal, List<LedgerEntry> entries) {

        policy.validate(entries);

        journal.markPosted(journal.getPostedBy());
        journal.getLedgerEntries().addAll(entries);

        JournalEntry saved = journalRepo.save(journal);

        // 🔥 UPDATE ACCOUNT BALANCES
        for (LedgerEntry entry : entries) {

            AccountBalance bal =
                    balanceRepo.findByAccount_Id(entry.getAccount().getId())
                            .orElseGet(() -> {
                                AccountBalance b = new AccountBalance();
                                b.setAccount(entry.getAccount());
                                b.setBalance(BigDecimal.ZERO);
                                return b;
                            });

            BigDecimal delta =
                    entry.getDirection() == EntryDirection.DEBIT
                            ? entry.getAmount()
                            : entry.getAmount().negate();

            bal.setBalance(bal.getBalance().add(delta));
            bal.setUpdatedAt(LocalDateTime.now());

            balanceRepo.save(bal);
        }

        return saved;
    }
}