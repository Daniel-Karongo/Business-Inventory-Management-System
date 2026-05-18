package com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.dto.SupplierBalanceSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierBalanceService {

    private final PurchaseInvoiceRepository
            purchaseInvoiceRepository;

    private final SupplierPaymentRepository
            supplierPaymentRepository;

    public SupplierBalanceSnapshot
    computeSupplierSnapshot(
            UUID branchId,
            UUID supplierId
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        BigDecimal outstandingInvoices =
                BigDecimal.ZERO;

        BigDecimal unappliedPayments =
                BigDecimal.ZERO;

        BigDecimal totalCredits =
                BigDecimal.ZERO;

        long overdueCount = 0L;

        long openInvoices = 0L;

        for (
                PurchaseInvoice invoice :
                purchaseInvoiceRepository.findAll()
        ) {

            if (
                    !invoice.getTenantId().equals(tenantId)
            ) {
                continue;
            }

            if (
                    !invoice.getBranchId().equals(branchId)
            ) {
                continue;
            }

            if (
                    !invoice.getSupplier().getId()
                            .equals(supplierId)
            ) {
                continue;
            }

            if (
                    invoice.getOutstandingAmount()
                            .compareTo(BigDecimal.ZERO) > 0
            ) {

                openInvoices++;

                outstandingInvoices =
                        outstandingInvoices.add(
                                invoice.getOutstandingAmount()
                        );

                if (
                        invoice.getDueDate()
                                .isBefore(LocalDate.now())
                ) {
                    overdueCount++;
                }
            }
        }

        for (
                SupplierPayment payment :
                supplierPaymentRepository.findAll()
        ) {

            if (
                    !payment.getTenantId().equals(tenantId)
            ) {
                continue;
            }

            if (
                    !payment.getBranchId().equals(branchId)
            ) {
                continue;
            }

            if (
                    !payment.getSupplierId()
                            .equals(supplierId)
            ) {
                continue;
            }

            unappliedPayments =
                    unappliedPayments.add(
                            payment.getUnappliedAmount()
                    );
        }

        return SupplierBalanceSnapshot
                .builder()
                .supplierId(supplierId)
                .totalOutstandingInvoices(
                        outstandingInvoices
                )
                .totalUnappliedPayments(
                        unappliedPayments
                )
                .totalCredits(totalCredits)
                .netPayablePosition(
                        outstandingInvoices.subtract(
                                unappliedPayments
                        )
                )
                .openInvoiceCount(openInvoices)
                .overdueInvoiceCount(overdueCount)
                .build();
    }
}