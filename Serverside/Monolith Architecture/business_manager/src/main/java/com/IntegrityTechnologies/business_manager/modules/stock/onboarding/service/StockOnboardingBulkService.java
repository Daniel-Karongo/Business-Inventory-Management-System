package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.exception.ExpectedConcurrencyException;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingBulkPreviewResult;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingBulkPreviewRow;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingResponse;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class StockOnboardingBulkService {

    private final StockOnboardingService onboardingService;

    @PersistenceContext
    private EntityManager entityManager;

    public StockOnboardingResponse executeRow(
            StockOnboardingRequest row
    ) {
        return onboardingService.onboard(row);
    }

    private String dedupeKey(
            StockOnboardingRequest row
    ) {

        String product =
                row.getProductName() != null
                        ? row.getProductName().trim().toLowerCase()
                        : String.valueOf(row.getProductId());

        String classification =
                row.getClassification() != null
                        ? row.getClassification().trim().toLowerCase()
                        : "standard";

        String packaging =
                row.getPackagings() != null
                        && !row.getPackagings().isEmpty()
                        && row.getPackagings().get(0).getName() != null
                        ? row.getPackagings()
                        .get(0)
                        .getName()
                        .trim()
                        .toLowerCase()
                        : "piece";

        String supplier =
                row.getSuppliers() != null
                        && !row.getSuppliers().isEmpty()
                        && row.getSuppliers().get(0).getSupplierName() != null
                        ? row.getSuppliers()
                        .get(0)
                        .getSupplierName()
                        .trim()
                        .toLowerCase()
                        : "unknown";

        return row.getBranchId()
                + "::"
                + product
                + "::"
                + classification
                + "::"
                + packaging
                + "::"
                + supplier;
    }

    @Transactional
    public BulkResult<StockOnboardingBulkPreviewResult> bulkOnboard(
            BulkRequest<StockOnboardingRequest> request
    ) {

        BulkResult<StockOnboardingBulkPreviewResult> result =
                new BulkResult<>();

        if (request == null
                || request.getItems() == null) {
            return result;
        }

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        result.setTotal(
                request.getItems().size()
        );

        List<StockOnboardingBulkPreviewRow> previewRows =
                new ArrayList<>();

        long totalUnits = 0;

        BigDecimal grossCost = BigDecimal.ZERO;
        BigDecimal netCost = BigDecimal.ZERO;
        BigDecimal vatAmount = BigDecimal.ZERO;

        Set<String> seenRows =
                new HashSet<>();

        /*
         * =====================================================
         * PHASE 1 — PREVALIDATION ONLY
         * =====================================================
         */

        for (int i = 0; i < request.getItems().size(); i++) {

            int rowNumber = i + 1;

            StockOnboardingRequest row =
                    request.getItems().get(i);

            try {

                if (options.isSkipDuplicates()) {

                    String dedupeKey =
                            dedupeKey(row);

                    if (!seenRows.add(dedupeKey)) {

                        result.addError(
                                rowNumber,
                                "Duplicate onboarding row detected"
                        );

                        continue;
                    }
                }

                StockOnboardingBulkPreviewRow preview =
                        onboardingService.preview(row);

                previewRows.add(preview);

                totalUnits +=
                        preview.getTotalUnits();

                grossCost =
                        grossCost.add(
                                preview.getGrossCost()
                        );

                netCost =
                        netCost.add(
                                preview.getNetCost()
                        );

                vatAmount =
                        vatAmount.add(
                                preview.getVatAmount()
                        );

            } catch (Exception ex) {

                result.addError(
                        rowNumber,
                        ex.getMessage()
                );
            }
        }

        /*
         * =====================================================
         * VALIDATION FAILED
         * =====================================================
         */

        if (!result.getErrors().isEmpty()) {

            result.getData().add(
                    StockOnboardingBulkPreviewResult
                            .builder()
                            .rows(previewRows)
                            .totalUnits(totalUnits)
                            .grossCost(grossCost)
                            .netCost(netCost)
                            .vatAmount(vatAmount)
                            .build()
            );

            return result;
        }

        /*
         * =====================================================
         * DRY RUN
         * =====================================================
         */

        if (options.isDryRun()) {

            result.setSuccess(
                    request.getItems().size()
            );

            result.getData().add(
                    StockOnboardingBulkPreviewResult
                            .builder()
                            .rows(previewRows)
                            .totalUnits(totalUnits)
                            .grossCost(grossCost)
                            .netCost(netCost)
                            .vatAmount(vatAmount)
                            .build()
            );

            return result;
        }

        /*
         * =====================================================
         * PHASE 2 — ATOMIC EXECUTION
         * =====================================================
         */

        for (StockOnboardingRequest row : request.getItems()) {

            onboardingService.onboard(row);

            entityManager.flush();
            entityManager.clear();
        }

        result.setSuccess(
                request.getItems().size()
        );

        result.getData().add(
                StockOnboardingBulkPreviewResult
                        .builder()
                        .rows(previewRows)
                        .totalUnits(totalUnits)
                        .grossCost(grossCost)
                        .netCost(netCost)
                        .vatAmount(vatAmount)
                        .build()
        );

        return result;
    }
}