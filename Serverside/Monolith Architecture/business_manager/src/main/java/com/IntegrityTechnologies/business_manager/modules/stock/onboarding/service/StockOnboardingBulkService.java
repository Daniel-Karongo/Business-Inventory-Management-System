package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.CreateOperationalExpenseRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.service.OperationalExpenseService;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.*;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

@Service
@RequiredArgsConstructor
public class StockOnboardingBulkService {

    private final StockOnboardingService onboardingService;
    private final StockOnboardingValidationService validationService;
    private final OperationalExpenseService operationalExpenseService;

    @PersistenceContext
    private EntityManager entityManager;

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
            BulkRequest<BulkStockOnboardingRequest> request
    ) {

        BulkResult<StockOnboardingBulkPreviewResult> result =
                new BulkResult<>();

        if (request == null
                || request.getItems() == null
                || request.getItems().isEmpty()) {
            return result;
        }

        if (request.getItems().size() != 1) {
            throw new IllegalArgumentException(
                    "Exactly one bulk payload is expected"
            );
        }

        BulkStockOnboardingRequest payload =
                request.getItems().get(0);

        List<StockOnboardingRequest> rows =
                payload.getRows();

        if (rows == null || rows.isEmpty()) {
            return result;
        }

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        result.setTotal(
                rows.size()
        );

        List<StockOnboardingBulkPreviewRow> previewRows =
                new ArrayList<>();

        long totalUnits = 0;

        BigDecimal grossCost = BigDecimal.ZERO;
        BigDecimal netCost = BigDecimal.ZERO;
        BigDecimal vatAmount = BigDecimal.ZERO;

        Set<String> seenRows =
                new HashSet<>();

        List<StockOnboardingRequest> validatedRows =
                new ArrayList<>();

        /*
         * =====================================================
         * PHASE 1 — PREVALIDATION ONLY
         * =====================================================
         */

        for (int i = 0; i < rows.size(); i++) {

            int rowNumber = i + 1;

            StockOnboardingRequest row =
                    rows.get(i);

            try {

                /*
                 * =====================================================
                 * DUPLICATES
                 * =====================================================
                 */

                if (options.isSkipDuplicates()) {

                    String dedupeKey =
                            dedupeKey(row);

                    if (!seenRows.add(dedupeKey)) {

                        result.addError(
                                rowNumber,
                                "row",
                                "DUPLICATE_ROW",
                                "Duplicate onboarding row detected",
                                null
                        );

                        continue;
                    }
                }

                /*
                 * =====================================================
                 * PREVIEW
                 * =====================================================
                 */

                StockOnboardingBulkPreviewRow preview =
                        onboardingService.preview(row);

                previewRows.add(preview);

                /*
                 * =====================================================
                 * BUSINESS VALIDATION
                 * =====================================================
                 */

                StockOnboardingRowValidationResult validation =
                        validationService.validate(
                                rowNumber,
                                row
                        );

                if (!validation.isValid()) {

                    validation.getErrors()
                            .forEach(error -> {

                                result.addError(
                                        error.getRow(),
                                        error.getField(),
                                        error.getCode(),
                                        error.getMessage(),
                                        error.getRejectedValue()
                                );
                            });

                    continue;
                }

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

                validatedRows.add(row);

            } catch (Exception ex) {

                result.addError(
                        rowNumber,
                        "row",
                        "ROW_VALIDATION_FAILED",
                        ex.getMessage(),
                        null
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
                    result.getTotal() - result.getFailed()
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

        for (StockOnboardingRequest row : validatedRows) {

            onboardingService.onboard(
                    stripExpenses(row)
            );

            entityManager.flush();
            entityManager.clear();
        }

        processBulkExpenses(
                payload
        );

        result.setSuccess(
                result.getTotal() - result.getFailed()
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

    private StockOnboardingRequest stripExpenses(
            StockOnboardingRequest source
    ) {
        StockOnboardingRequest copy =
                new StockOnboardingRequest();

        BeanUtils.copyProperties(
                source,
                copy
        );

        copy.setOperationalExpenses(null);
        copy.setAutoPayOperationalExpenses(false);

        return copy;
    }

    private void processBulkExpenses(
            BulkStockOnboardingRequest payload
    ) {
        if (
                payload.getOperationalExpenses() == null
                        || payload.getOperationalExpenses().isEmpty()
        ) {
            return;
        }

        UUID branchId = null;
        LocalDate accountingDate = null;

        if (
                payload.getRows() != null
                        && !payload.getRows().isEmpty()
        ) {
            branchId =
                    payload.getRows()
                            .get(0)
                            .getBranchId();

            accountingDate =
                    payload.getRows()
                            .get(0)
                            .getAccountingDate();
        }

        int index = 0;

        for (var expense :
                payload.getOperationalExpenses()) {

            CreateOperationalExpenseRequest create =
                    new CreateOperationalExpenseRequest();

            create.setBranchId(
                    branchId
            );

            create.setExpenseAccountId(
                    expense.getExpenseAccountId()
            );

            create.setFundingAccountId(
                    payload.getFundingAccountId()
            );

            create.setDescription(
                    expense.getDescription()
            );

            create.setAmount(
                    expense.getAmount()
            );

            create.setAccountingDate(
                    accountingDate
            );

            create.setAutoPay(
                    Boolean.TRUE.equals(
                            payload.getAutoPayOperationalExpenses()
                    )
            );

            create.setReference(
                    "BULK-EXPENSE-" + index
            );

            create.setSourceModule(
                    "BULK_STOCK_ONBOARDING"
            );

            create.setSourceId(
                    UUID.nameUUIDFromBytes(
                            (
                                    "BULK-EXPENSE-"
                                            + index
                            ).getBytes()
                    )
            );

            operationalExpenseService
                    .createExpense(
                            create
                    );

            index++;
        }
    }
}