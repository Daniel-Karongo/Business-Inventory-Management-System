package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository.SupplierPaymentAllocationRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto.SupplierStatementEntryDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.enums.SupplierDocumentType;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;

@Service
@RequiredArgsConstructor
public class SupplierStatementService {

    private final PurchaseInvoiceRepository invoiceRepository;
    private final SupplierPaymentRepository paymentRepository;
    private final SupplierPaymentAllocationRepository allocationRepository;
    private final BranchRepository branchRepository;
    private final BranchTenantGuard branchTenantGuard;

    public List<SupplierStatementEntryDto>
    generateStatement(
            UUID branchId,
            UUID supplierId
    ) {

        branchTenantGuard.validate(branchId);

        return build(
                invoiceRepository
                        .findByTenantIdAndBranchIdAndSupplier_Id(
                                TenantContext.getTenantId(),
                                branchId,
                                supplierId
                        ),

                paymentRepository
                        .findByTenantIdAndBranchIdAndSupplierId(
                                TenantContext.getTenantId(),
                                branchId,
                                supplierId
                        ),

                false
        );
    }

    public List<SupplierStatementEntryDto>
    generateConsolidatedStatement(
            UUID supplierId
    ) {

        return build(
                invoiceRepository
                        .findByTenantIdAndSupplier_Id(
                                TenantContext.getTenantId(),
                                supplierId
                        ),

                paymentRepository
                        .findByTenantIdAndSupplierId(
                                TenantContext.getTenantId(),
                                supplierId
                        ),

                true
        );
    }

    private List<SupplierStatementEntryDto>
    build(
            List<PurchaseInvoice> invoices,
            List<SupplierPayment> payments,
            boolean includeBranch
    ) {

        List<SupplierStatementEntryDto>
                entries =
                new ArrayList<>();

        for (PurchaseInvoice invoice : invoices) {

            String branchName =
                    includeBranch
                            ? branchRepository
                            .findById(invoice.getBranchId())
                            .map(branch -> branch.getName())
                            .orElse("Unknown Branch")
                            : null;

            entries.add(
                    SupplierStatementEntryDto
                            .builder()
                            .transactionDate(
                                    invoice.getCreatedAt()
                            )
                            .branchName(
                                    branchName
                            )
                            .documentType(
                                    SupplierDocumentType
                                            .PURCHASE_INVOICE
                                            .name()
                            )
                            .documentId(
                                    invoice.getId()
                            )
                            .documentNumber(
                                    invoice.getDocumentNumber()
                            )
                            .referenceNumber(
                                    invoice.getSupplierInvoiceNumber()
                            )
                            .description(
                                    "Purchase Invoice"
                            )
                            .debitAmount(
                                    invoice.getTotalAmount()
                            )
                            .creditAmount(
                                    BigDecimal.ZERO
                            )
                            .build()
            );
        }

        for (SupplierPayment payment : payments) {

            String branchName =
                    includeBranch
                            ? branchRepository
                            .findById(payment.getBranchId())
                            .map(branch -> branch.getName())
                            .orElse("Unknown Branch")
                            : null;

            entries.add(
                    SupplierStatementEntryDto
                            .builder()
                            .transactionDate(
                                    payment.getPaidAt()
                            )
                            .branchName(
                                    branchName
                            )
                            .documentType(
                                    SupplierDocumentType
                                            .SUPPLIER_PAYMENT
                                            .name()
                            )
                            .documentId(
                                    payment.getId()
                            )
                            .documentNumber(
                                    payment.getDocumentNumber()
                            )
                            .referenceNumber(
                                    payment.getReference()
                            )
                            .description(
                                    "Supplier Payment"
                            )
                            .debitAmount(
                                    BigDecimal.ZERO
                            )
                            .creditAmount(
                                    payment.getAmount()
                            )
                            .build()
            );
        }

        entries.sort(
                Comparator.comparing(
                        SupplierStatementEntryDto::getTransactionDate
                )
        );

        if (includeBranch) {

            Map<String, BigDecimal>
                    balances =
                    new HashMap<>();

            for (
                    SupplierStatementEntryDto entry :
                    entries
            ) {

                String branch =
                        entry.getBranchName();

                BigDecimal running =
                        balances.getOrDefault(
                                branch,
                                BigDecimal.ZERO
                        );

                running =
                        running.add(
                                entry.getDebitAmount()
                        );

                running =
                        running.subtract(
                                entry.getCreditAmount()
                        );

                entry.setRunningBalance(
                        running
                );

                balances.put(
                        branch,
                        running
                );
            }

        } else {

            BigDecimal running =
                    BigDecimal.ZERO;

            for (
                    SupplierStatementEntryDto entry :
                    entries
            ) {

                running =
                        running.add(
                                entry.getDebitAmount()
                        );

                running =
                        running.subtract(
                                entry.getCreditAmount()
                        );

                entry.setRunningBalance(
                        running
                );
            }
        }

        return entries;
    }
}