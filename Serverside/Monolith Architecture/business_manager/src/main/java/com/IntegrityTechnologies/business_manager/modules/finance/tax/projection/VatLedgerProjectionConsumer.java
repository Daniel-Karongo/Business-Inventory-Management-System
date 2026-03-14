package com.IntegrityTechnologies.business_manager.modules.finance.tax.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatLedgerProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;

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
public class VatLedgerProjectionConsumer {

    private final VatLedgerProjectionRepository repo;
    private final AccountingAccounts accounts;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "vat-ledger"
    )
    @Transactional
    @ConditionalOnProperty(
            name = "spring.kafka.enabled",
            havingValue = "true",
            matchIfMissing = false
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

            UUID outputVat =
                    accounts.get(tenantId, branchId, AccountRole.VAT_OUTPUT);

            UUID inputVat =
                    accounts.get(tenantId, branchId, AccountRole.VAT_INPUT);

            int year = LocalDate.now().getYear();
            int month = LocalDate.now().getMonthValue();

            VatLedgerProjection projection =
                    repo.findByTenantIdAndBranchIdAndFiscalYearAndMonthNumber(
                            tenantId,
                            branchId,
                            year,
                            month
                    ).orElse(
                            VatLedgerProjection.builder()
                                    .tenantId(tenantId)
                                    .branchId(branchId)
                                    .fiscalYear(year)
                                    .monthNumber(month)
                                    .outputVat(BigDecimal.ZERO)
                                    .inputVat(BigDecimal.ZERO)
                                    .build()
                    );

            for (LedgerEntryDTO entry : event.entries()) {

                if (!entry.accountId().equals(outputVat)
                        && !entry.accountId().equals(inputVat))
                    continue;

                BigDecimal delta =
                        entry.direction() == EntryDirection.DEBIT
                                ? entry.amount()
                                : entry.amount().negate();

                if (entry.accountId().equals(outputVat)) {

                    projection.setOutputVat(
                            projection.getOutputVat().add(delta)
                    );

                } else {

                    projection.setInputVat(
                            projection.getInputVat().add(delta)
                    );
                }
            }

            repo.save(projection);

        } finally {

            TenantContext.clear();
        }
    }
}