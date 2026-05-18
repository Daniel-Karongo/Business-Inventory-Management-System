package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto.SupplierAgingBucketDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto.SupplierAgingRowDto;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.*;

@Service
@RequiredArgsConstructor
public class AccountsPayableAgingService {

    private final PurchaseInvoiceRepository repository;
    private final BranchRepository branchRepository;
    private final BranchTenantGuard branchTenantGuard;

    public List<SupplierAgingRowDto>
    generateAging(UUID branchId) {

        branchTenantGuard.validate(branchId);

        return build(
                repository.findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId
                ),
                false
        );
    }

    public List<SupplierAgingRowDto>
    generateConsolidatedAging() {

        return build(
                repository.findByTenantId(
                        TenantContext.getTenantId()
                ),
                true
        );
    }

    private List<SupplierAgingRowDto>
    build(
            List<PurchaseInvoice> invoices,
            boolean includeBranch
    ) {

        Map<String, SupplierAgingBucketDto>
                bucketMap =
                new HashMap<>();

        Map<String, String>
                supplierNames =
                new HashMap<>();

        Map<String, String>
                branchNames =
                new HashMap<>();

        for (PurchaseInvoice invoice : invoices) {

            if (
                    invoice.getOutstandingAmount()
                            .compareTo(BigDecimal.ZERO) <= 0
            ) {
                continue;
            }

            UUID supplierId =
                    invoice.getSupplier().getId();

            UUID branchId =
                    invoice.getBranchId();

            String key =
                    includeBranch
                            ? branchId + "_" + supplierId
                            : supplierId.toString();

            supplierNames.put(
                    key,
                    invoice.getSupplier().getName()
            );

            if (includeBranch) {

                String branchName =
                        branchRepository
                                .findById(branchId)
                                .map(branch -> branch.getName())
                                .orElse("Unknown Branch");

                branchNames.put(
                        key,
                        branchName
                );
            }

            SupplierAgingBucketDto bucket =
                    bucketMap.computeIfAbsent(
                            key,
                            id -> SupplierAgingBucketDto
                                    .builder()
                                    .current(BigDecimal.ZERO)
                                    .days1To30(BigDecimal.ZERO)
                                    .days31To60(BigDecimal.ZERO)
                                    .days61To90(BigDecimal.ZERO)
                                    .days91To120(BigDecimal.ZERO)
                                    .over120Days(BigDecimal.ZERO)
                                    .totalOutstanding(BigDecimal.ZERO)
                                    .build()
                    );

            long age =
                    ChronoUnit.DAYS.between(
                            invoice.getDueDate(),
                            LocalDate.now()
                    );

            BigDecimal amount =
                    invoice.getOutstandingAmount();

            if (age <= 0) {

                bucket.setCurrent(
                        bucket.getCurrent().add(amount)
                );

            } else if (age <= 30) {

                bucket.setDays1To30(
                        bucket.getDays1To30().add(amount)
                );

            } else if (age <= 60) {

                bucket.setDays31To60(
                        bucket.getDays31To60().add(amount)
                );

            } else if (age <= 90) {

                bucket.setDays61To90(
                        bucket.getDays61To90().add(amount)
                );

            } else if (age <= 120) {

                bucket.setDays91To120(
                        bucket.getDays91To120().add(amount)
                );

            } else {

                bucket.setOver120Days(
                        bucket.getOver120Days().add(amount)
                );
            }

            bucket.setTotalOutstanding(
                    bucket.getTotalOutstanding().add(amount)
            );
        }

        List<SupplierAgingRowDto>
                rows =
                new ArrayList<>();

        for (
                Map.Entry<String, SupplierAgingBucketDto>
                        entry :
                bucketMap.entrySet()
        ) {

            SupplierAgingBucketDto bucket =
                    entry.getValue();

            rows.add(
                    SupplierAgingRowDto
                            .builder()
                            .supplierName(
                                    supplierNames.get(
                                            entry.getKey()
                                    )
                            )
                            .branchName(
                                    branchNames.get(
                                            entry.getKey()
                                    )
                            )
                            .current(
                                    bucket.getCurrent()
                            )
                            .days1To30(
                                    bucket.getDays1To30()
                            )
                            .days31To60(
                                    bucket.getDays31To60()
                            )
                            .days61To90(
                                    bucket.getDays61To90()
                            )
                            .days91To120(
                                    bucket.getDays91To120()
                            )
                            .over120Days(
                                    bucket.getOver120Days()
                            )
                            .totalOutstanding(
                                    bucket.getTotalOutstanding()
                            )
                            .build()
            );
        }

        rows.sort(
                Comparator.comparing(
                        SupplierAgingRowDto::getBranchName,
                        Comparator.nullsLast(String::compareTo)
                ).thenComparing(
                        SupplierAgingRowDto::getTotalOutstanding
                ).reversed()
        );

        return rows;
    }
}