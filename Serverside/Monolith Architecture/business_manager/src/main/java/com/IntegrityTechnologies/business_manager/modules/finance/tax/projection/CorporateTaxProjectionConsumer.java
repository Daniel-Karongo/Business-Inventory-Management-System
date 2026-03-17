package com.IntegrityTechnologies.business_manager.modules.finance.tax.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxLedgerProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;

import lombok.RequiredArgsConstructor;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CorporateTaxProjectionConsumer {

    private final CorporateTaxLedgerProjectionRepository repo;
    private final AccountRepository accountRepository;
    private final TaxProperties taxProperties;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "corporate-tax-projection"
    )
    @Transactional
    @ConditionalOnProperty(
            name="spring.kafka.enabled",
            havingValue="true",
            matchIfMissing=false
    )
    public void handleKafka(JournalPostedEvent event) {
        process(event);
    }

    @EventListener
    @Transactional
    public void handleSpring(JournalPostedEvent event) {
        process(event);
    }

    private void process(JournalPostedEvent event) {

        try {

            TenantContext.setTenantId(event.tenantId());

            UUID tenantId = event.tenantId();
            UUID branchId = event.branchId();

            int year = LocalDate.now().getYear();
            int month = LocalDate.now().getMonthValue();

            CorporateTaxLedgerProjection projection =
                    repo.findByTenantIdAndBranchIdAndFiscalYearAndMonthNumber(
                            tenantId,
                            branchId,
                            year,
                            month
                    ).orElse(
                            CorporateTaxLedgerProjection.builder()
                                    .tenantId(tenantId)
                                    .branchId(branchId)
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
                        accountRepository
                                .findByTenantIdAndBranchIdAndId(
                                        tenantId,
                                        branchId,
                                        entry.accountId()
                                )
                                .orElse(null);

                if (account == null) continue;

                BigDecimal delta =
                        entry.direction() == EntryDirection.DEBIT
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

        } finally {

            TenantContext.clear();
        }
    }
}