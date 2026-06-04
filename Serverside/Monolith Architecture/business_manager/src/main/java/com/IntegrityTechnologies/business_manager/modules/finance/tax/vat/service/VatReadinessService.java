package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingReadinessResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatReadinessService {

    private final VatFilingRepository filingRepository;

    public VatFilingReadinessResponse check(
            AccountingPeriod period,
            UUID branchId
    ) {

        boolean closed =
                period.isClosed();

        boolean alreadyFiled =
                filingRepository
                        .existsByTenantIdAndPeriod_IdAndBranchId(
                                TenantContext.getTenantId(),
                                period.getId(),
                                branchId
                        );

        boolean ready =
                closed
                        &&
                        !alreadyFiled;

        List<String> warnings = new ArrayList<>();

        if (period.getReopenedAt() != null) {

            warnings.add(
                    "This accounting period has been reopened."
            );
        }

        if (!closed) {

            warnings.add(
                    "VAT filing is only available after period closure."
            );
        }

        if (alreadyFiled) {

            warnings.add(
                    "A VAT filing already exists for this period."
            );
        }

        String message;

        if (!closed) {

            message =
                    "Period not closed";
        }
        else if (alreadyFiled) {

            message =
                    "Already filed";
        }
        else {

            message =
                    "Ready to file";
        }

        return VatFilingReadinessResponse
                .builder()
                .ready(
                        ready
                )
                .periodClosed(
                        closed
                )
                .alreadyFiled(
                        alreadyFiled
                )
                .message(
                        message
                )
                .warnings(
                        warnings
                )
                .build();
    }
}