package com.IntegrityTechnologies.business_manager.modules.finance.tax.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxLedgerProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxLedgerProjectionRepository;
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
public class CorporateTaxProjectionConsumer {

    private final ObjectMapper mapper;
    private final CorporateTaxLedgerProjectionRepository repo;
    private final AccountRepository accountRepository;
    private final TaxProperties taxProperties;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "corporate-tax-projection"
    )
    @Transactional
    public void handle(String payload) {

        try {

            JournalPostedEvent event =
                    mapper.readValue(payload, JournalPostedEvent.class);

            int year = LocalDate.now().getYear();
            int month = LocalDate.now().getMonthValue();

            CorporateTaxLedgerProjection projection =
                    repo.findByBranchIdAndFiscalYearAndMonthNumber(
                            event.branchId(),
                            year,
                            month
                    ).orElse(
                            CorporateTaxLedgerProjection.builder()
                                    .branchId(event.branchId())
                                    .fiscalYear(year)
                                    .monthNumber(month)
                                    .revenue(BigDecimal.ZERO)
                                    .expenses(BigDecimal.ZERO)
                                    .taxableProfit(BigDecimal.ZERO)
                                    .estimatedTax(BigDecimal.ZERO)
                                    .build()
                    );

            for (LedgerEntryDTO entry : event.entries()) {

                var account =
                        accountRepository.findById(entry.accountId()).orElse(null);

                if (account == null) continue;

                BigDecimal delta =
                        entry.direction().name().equals("DEBIT")
                                ? entry.amount()
                                : entry.amount().negate();

                if (account.getType() == AccountType.INCOME) {

                    projection.setRevenue(
                            projection.getRevenue().add(delta)
                    );

                } else if (account.getType() == AccountType.EXPENSE) {

                    projection.setExpenses(
                            projection.getExpenses().add(delta)
                    );
                }
            }

            BigDecimal profit =
                    projection.getRevenue()
                            .subtract(projection.getExpenses());

            projection.setTaxableProfit(profit);

            if (profit.compareTo(BigDecimal.ZERO) > 0) {

                projection.setEstimatedTax(
                        profit.multiply(taxProperties.getCorporateTaxRate())
                );

            } else {

                projection.setEstimatedTax(BigDecimal.ZERO);
            }

            repo.save(projection);

        } catch (Exception ex) {

            throw new RuntimeException("Corporate tax projection failed", ex);
        }
    }
}