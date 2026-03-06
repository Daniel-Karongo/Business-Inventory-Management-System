package com.IntegrityTechnologies.business_manager.modules.finance.tax.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatLedgerProjection;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class VatLedgerProjectionConsumer {

    private final ObjectMapper mapper;
    private final VatLedgerProjectionRepository repo;
    private final AccountingAccounts accounts;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "vat-ledger"
    )
    @Transactional
    public void handle(String payload) {

        try {

            JournalPostedEvent event =
                    mapper.readValue(payload, JournalPostedEvent.class);

            UUID outputVat = accounts.get(event.branchId(), AccountRole.VAT_OUTPUT);
            UUID inputVat = accounts.get(event.branchId(), AccountRole.VAT_INPUT);

            for (LedgerEntryDTO entry : event.entries()) {

                if (!entry.accountId().equals(outputVat)
                        && !entry.accountId().equals(inputVat))
                    continue;

                BigDecimal delta =
                        entry.direction().name().equals("DEBIT")
                                ? entry.amount()
                                : entry.amount().negate();

                int year = LocalDate.now().getYear();
                int month = LocalDate.now().getMonthValue();

                VatLedgerProjection projection =
                        repo.findByBranchIdAndFiscalYearAndMonthNumber(
                                event.branchId(),
                                year,
                                month
                        ).orElse(
                                VatLedgerProjection.builder()
                                        .branchId(event.branchId())
                                        .fiscalYear(year)
                                        .monthNumber(month)
                                        .outputVat(BigDecimal.ZERO)
                                        .inputVat(BigDecimal.ZERO)
                                        .build()
                        );

                if (entry.accountId().equals(outputVat)) {

                    projection.setOutputVat(
                            projection.getOutputVat().add(delta)
                    );

                } else {

                    projection.setInputVat(
                            projection.getInputVat().add(delta)
                    );
                }

                repo.save(projection);
            }

        } catch (Exception ex) {

            throw new RuntimeException("VAT projection failed", ex);
        }
    }
}