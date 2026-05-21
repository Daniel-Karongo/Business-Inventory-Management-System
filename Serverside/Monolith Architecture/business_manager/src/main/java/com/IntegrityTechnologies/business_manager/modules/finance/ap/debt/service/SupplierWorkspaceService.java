package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.repository.SupplierWorkspaceQueryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SupplierWorkspaceService {

    private final SupplierRepository supplierRepository;
    private final SupplierWorkspaceQueryRepository repository;

    public SupplierWorkspaceDto getWorkspace(
            UUID branchId,
            UUID supplierId
    ) {

        UUID tenantId = TenantContext.getTenantId();

        Supplier supplier =
                supplierRepository.findActiveById(
                                supplierId,
                                tenantId,
                                branchId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Supplier not found"
                                )
                        );

        List<SupplierBillProjection> bills =
                repository.findBills(
                        tenantId,
                        branchId,
                        supplierId
                );

        List<SupplierBillLineProjection> lines =
                repository.findBillLines(
                        tenantId,
                        branchId,
                        supplierId
                );

        List<InvoiceSettlementProjection> settlements =
                repository.findInvoiceSettlements(
                        tenantId,
                        branchId,
                        supplierId
                );

        List<SupplierPaymentProjection> payments =
                repository.findPayments(
                        tenantId,
                        branchId,
                        supplierId
                );

        List<PaymentSettlementProjection> paymentSettlements =
                repository.findPaymentSettlements(
                        tenantId,
                        branchId,
                        supplierId
                );

        List<SupplierTimelineProjection> timelineProjections =
                repository.findTimeline(
                        tenantId,
                        branchId,
                        supplierId
                );

        Map<UUID, List<SupplierBillLineProjection>> linesByInvoice =
                lines.stream()
                        .collect(
                                Collectors.groupingBy(
                                        SupplierBillLineProjection::getInvoiceId
                                )
                        );

        Map<UUID, List<InvoiceSettlementProjection>> settlementsByInvoice =
                settlements.stream()
                        .collect(
                                Collectors.groupingBy(
                                        InvoiceSettlementProjection::getInvoiceId
                                )
                        );

        Map<UUID, List<PaymentSettlementProjection>> settlementsByPayment =
                paymentSettlements.stream()
                        .collect(
                                Collectors.groupingBy(
                                        PaymentSettlementProjection::getPaymentId
                                )
                        );

        List<SupplierBillDto> billDtos =
                bills.stream()
                        .map(bill -> mapBill(
                                bill,
                                linesByInvoice.getOrDefault(
                                        bill.getInvoiceId(),
                                        List.of()
                                ),
                                settlementsByInvoice.getOrDefault(
                                        bill.getInvoiceId(),
                                        List.of()
                                )
                        ))
                        .toList();

        List<SupplierPaymentDto> paymentDtos =
                payments.stream()
                        .map(payment -> mapPayment(
                                payment,
                                settlementsByPayment.getOrDefault(
                                        payment.getPaymentId(),
                                        List.of()
                                )
                        ))
                        .toList();

        List<SupplierTimelineEntryDto> timeline =
                buildTimeline(
                        timelineProjections
                );

        BigDecimal totalOutstanding =
                billDtos.stream()
                        .map(SupplierBillDto::getRemainingAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal overdueAmount =
                billDtos.stream()
                        .filter(SupplierBillDto::isOverdue)
                        .map(SupplierBillDto::getRemainingAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal unappliedPayments =
                paymentDtos.stream()
                        .map(SupplierPaymentDto::getUnappliedAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        long overdueBills =
                billDtos.stream()
                        .filter(SupplierBillDto::isOverdue)
                        .count();

        return SupplierWorkspaceDto.builder()
                .supplierId(supplierId)
                .supplierName(supplier.getName())
                .totalOutstanding(totalOutstanding)
                .overdueAmount(overdueAmount)
                .unappliedPayments(unappliedPayments)
                .netPayable(
                        totalOutstanding.subtract(unappliedPayments)
                )
                .openBills(billDtos.size())
                .overdueBills(overdueBills)
                .bills(billDtos)
                .payments(paymentDtos)
                .timeline(timeline)
                .build();
    }

    private SupplierBillDto mapBill(
            SupplierBillProjection bill,
            List<SupplierBillLineProjection> lines,
            List<InvoiceSettlementProjection> settlements
    ) {

        return SupplierBillDto.builder()
                .invoiceId(bill.getInvoiceId())
                .billNumber(bill.getBillNumber())
                .invoiceDate(bill.getInvoiceDate())
                .dueDate(bill.getDueDate())
                .totalAmount(bill.getTotalAmount())
                .paidAmount(bill.getPaidAmount())
                .remainingAmount(bill.getRemainingAmount())
                .overdue(
                        bill.getRemainingAmount().compareTo(BigDecimal.ZERO) > 0
                                &&
                                bill.getDueDate().isBefore(LocalDate.now())
                )
                .status(bill.getStatus())
                .items(
                        lines.stream()
                                .map(this::mapLine)
                                .toList()
                )
                .settlements(
                        settlements.stream()
                                .map(this::mapInvoiceSettlement)
                                .toList()
                )
                .build();
    }

    private SupplierBillLineDto mapLine(
            SupplierBillLineProjection line
    ) {

        return SupplierBillLineDto.builder()
                .productName(line.getProductName())
                .variantName(line.getVariantName())
                .quantity(line.getQuantity())
                .unitCost(line.getUnitCost())
                .totalCost(line.getTotalCost())
                .build();
    }

    private InvoiceSettlementDto mapInvoiceSettlement(
            InvoiceSettlementProjection settlement
    ) {

        return InvoiceSettlementDto.builder()
                .paymentId(settlement.getPaymentId())
                .paymentNumber(settlement.getPaymentNumber())
                .paymentDate(settlement.getPaymentDate())
                .allocatedAmount(settlement.getAllocatedAmount())
                .paymentMethod(settlement.getPaymentMethod())
                .build();
    }

    private SupplierPaymentDto mapPayment(
            SupplierPaymentProjection payment,
            List<PaymentSettlementProjection> settlements
    ) {

        return SupplierPaymentDto.builder()
                .paymentId(payment.getPaymentId())
                .paymentNumber(payment.getPaymentNumber())
                .paymentDate(payment.getPaymentDate())
                .amount(payment.getAmount())
                .allocatedAmount(payment.getAllocatedAmount())
                .unappliedAmount(payment.getUnappliedAmount())
                .paymentMethod(payment.getPaymentMethod())
                .reference(payment.getReference())
                .allocations(
                        settlements.stream()
                                .map(this::mapPaymentSettlement)
                                .toList()
                )
                .build();
    }

    private PaymentSettlementDto mapPaymentSettlement(
            PaymentSettlementProjection settlement
    ) {

        return PaymentSettlementDto.builder()
                .invoiceId(settlement.getInvoiceId())
                .billNumber(settlement.getBillNumber())
                .invoiceDate(settlement.getInvoiceDate())
                .allocatedAmount(settlement.getAllocatedAmount())
                .build();
    }

    private List<SupplierTimelineEntryDto> buildTimeline(
            List<SupplierTimelineProjection> projections
    ) {

        List<SupplierTimelineEntryDto> entries =
                projections.stream()
                        .map(projection ->
                                SupplierTimelineEntryDto.builder()
                                        .timestamp(projection.getTimestamp())
                                        .activity(projection.getActivity())
                                        .reference(projection.getReference())
                                        .debitAmount(projection.getDebitAmount())
                                        .creditAmount(projection.getCreditAmount())
                                        .build()
                        )
                        .sorted(
                                Comparator.comparing(
                                        SupplierTimelineEntryDto::getTimestamp
                                )
                        )
                        .toList();

        BigDecimal running = BigDecimal.ZERO;

        List<SupplierTimelineEntryDto> computed =
                new ArrayList<>();

        for (SupplierTimelineEntryDto entry : entries) {

            running =
                    running
                            .add(entry.getDebitAmount())
                            .subtract(entry.getCreditAmount());

            entry.setRunningBalance(running);

            computed.add(entry);
        }

        return computed;
    }
}