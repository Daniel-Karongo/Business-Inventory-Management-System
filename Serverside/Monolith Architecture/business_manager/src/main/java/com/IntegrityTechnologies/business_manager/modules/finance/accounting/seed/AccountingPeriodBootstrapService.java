package com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingPeriodBootstrapService {

    private final AccountingPeriodRepository repository;

    public void ensureCurrentPeriod(UUID tenantId, UUID branchId) {

        LocalDate now = LocalDate.now();

        AccountingPeriod existing =
                repository.findByTenantIdAndBranchIdAndStartDateLessThanEqualAndEndDateGreaterThanEqual(
                                tenantId,
                                branchId,
                                now,
                                now
                        )
                        .orElse(null);

        if (existing != null) {
            return;
        }

        LocalDate start = now.withDayOfMonth(1);

        LocalDate end = start.plusMonths(1).minusDays(1);

        repository.save(
                AccountingPeriod.builder()
                        .tenantId(tenantId)
                        .branchId(branchId)
                        .startDate(start)
                        .endDate(end)
                        .closed(false)
                        .taxAccrued(false)
                        .build()
        );
    }
}