package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.projection;

import com.IntegrityTechnologies.business_manager.config.kafka.ProcessedKafkaEventRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CorporateTaxProjectionConsumer {

    private final CorporateTaxLedgerProjectionRepository repo;
    private final AccountRepository accountRepository;
    private final TaxSystemStateService taxSystemStateService;
    private final ProcessedKafkaEventRepository processedRepo;

    private static final String CONSUMER = "CORPORATE_TAX";

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
    @ConditionalOnProperty(
            name = "spring.kafka.enabled",
            havingValue = "false",
            matchIfMissing = true
    )
    public void handleSpring(JournalPostedEvent event) {
        process(event);
    }

    private void process(JournalPostedEvent event) {

        TenantContext.setTenantId(event.tenantId());

        int claimed =
                processedRepo.tryClaim(
                        UUID.randomUUID(),
                        event.tenantId(),
                        event.journalId(),
                        CONSUMER
                );

        if (claimed == 0) {
            return;
        }

        try {

            UUID tenantId = event.tenantId();
            UUID branchId = event.branchId();

            int year = event.accountingDate().getYear();
            int month = event.accountingDate().getMonthValue();

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

                if (account == null) {
                    continue;
                }

                if (account.getType() == AccountType.INCOME) {

                    BigDecimal revenueDelta =
                            entry.direction() == EntryDirection.CREDIT
                                    ? entry.amount()
                                    : entry.amount().negate();

                    projection.setRevenue(
                            projection.getRevenue().add(revenueDelta)
                    );

                } else if (account.getType() == AccountType.EXPENSE) {

                    if ("CORPORATE_TAX_EXPENSE".equals(account.getRole())) {
                        continue;
                    }

                    BigDecimal expenseDelta =
                            entry.direction() == EntryDirection.DEBIT
                                    ? entry.amount()
                                    : entry.amount().negate();

                    projection.setExpenses(
                            projection.getExpenses().add(expenseDelta)
                    );
                }
            }

            BigDecimal profit =
                    projection.getRevenue()
                            .subtract(projection.getExpenses());

            projection.setTaxableProfit(profit);

            if (profit.compareTo(BigDecimal.ZERO) > 0) {

                BigDecimal corporateTaxRate =
                        taxSystemStateService
                                .getOrCreate(branchId)
                                .getCorporateTaxRate();

                projection.setEstimatedTax(
                        profit.multiply(
                                corporateTaxRate
                        )
                );

            } else {

                projection.setEstimatedTax(
                        BigDecimal.ZERO
                );

            }

            repo.save(projection);

        } finally {
            TenantContext.clear();
        }
    }
}