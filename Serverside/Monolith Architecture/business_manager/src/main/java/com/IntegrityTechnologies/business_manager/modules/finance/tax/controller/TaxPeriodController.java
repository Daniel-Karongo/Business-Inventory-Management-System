package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.TaxPeriodDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.mapper.TaxPeriodMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/periods")
@RequiredArgsConstructor
@TenantManagerOnly
public class TaxPeriodController {

    private final TaxPeriodRepository repository;
    private final BranchTenantGuard branchTenantGuard;
    private final GovernanceAuditService auditService;

    @GetMapping
    public Page<TaxPeriodDTO> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable =
                PageRequest.of(page, size);

        return repository
                .findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId,
                        pageable
                )
                .map(TaxPeriodMapper::toDto);
    }

    @GetMapping("/current")
    public TaxPeriodDTO current(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        return repository
                .findByTenantIdAndBranchIdAndClosedFalse(
                        TenantContext.getTenantId(),
                        branchId
                )
                .map(TaxPeriodMapper::toDto)
                .orElse(null);
    }

    @PostMapping
    public TaxPeriodDTO create(
            @RequestParam UUID branchId,
            @RequestParam LocalDate startDate,
            @RequestParam LocalDate endDate
    ) {

        branchTenantGuard.validate(branchId);

        repository
                .findByTenantIdAndBranchIdAndClosedFalse(
                        TenantContext.getTenantId(),
                        branchId
                )
                .ifPresent(x -> {
                    throw new IllegalStateException(
                            "An open tax period already exists."
                    );
                });

        TaxPeriod period =
                TaxPeriod.builder()
                        .tenantId(
                                TenantContext.getTenantId()
                        )
                        .branchId(branchId)
                        .startDate(startDate)
                        .endDate(endDate)
                        .closed(false)
                        .build();

        auditService.log(
                branchId,
                "TAX_PERIOD_CREATED",
                SecurityUtils.currentUsername(),
                startDate + " -> " + endDate
        );

        return TaxPeriodMapper.toDto(
                repository.save(period)
        );
    }

    @PostMapping("/{periodId}/close")
    public void close(
            @PathVariable UUID periodId
    ) {

        TaxPeriod period =
                repository
                        .findByTenantIdAndId(
                                TenantContext.getTenantId(),
                                periodId
                        )
                        .orElseThrow();

        period.setClosed(true);

        period.setClosedBy(
                SecurityUtils.currentUsername()
        );

        auditService.log(
                period.getBranchId(),
                "TAX_PERIOD_CLOSED",
                SecurityUtils.currentUsername(),
                period.getId().toString()
        );

        repository.save(period);
    }
}