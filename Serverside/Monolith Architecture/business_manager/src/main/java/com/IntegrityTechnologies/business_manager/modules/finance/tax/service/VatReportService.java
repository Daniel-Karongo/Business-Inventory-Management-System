package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;

@Service
@RequiredArgsConstructor
public class VatReportService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountingAccounts accountingAccounts;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public VatReport generate(LocalDateTime from, LocalDateTime to, UUID branchId) {

        UUID outputVatId = accountingAccounts.get(tenantId(), branchId, AccountRole.VAT_OUTPUT);
        UUID inputVatId = accountingAccounts.get(tenantId(), branchId, AccountRole.VAT_INPUT);

        BigDecimal outputVat =
                ledgerRepo.netMovementForAccount(
                        tenantId(),
                        outputVatId,
                        from,
                        to,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal inputVat =
                ledgerRepo.netMovementForAccount(
                        tenantId(),
                        inputVatId,
                        from,
                        to,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal payable = outputVat.subtract(inputVat);

        return new VatReport(outputVat, inputVat, payable);
    }

    public record VatReport(
            BigDecimal outputVat,
            BigDecimal inputVat,
            BigDecimal vatPayable
    ) {}
}