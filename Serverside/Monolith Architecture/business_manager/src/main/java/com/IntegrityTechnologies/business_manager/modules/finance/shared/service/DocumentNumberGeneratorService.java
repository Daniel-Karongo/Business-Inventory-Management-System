package com.IntegrityTechnologies.business_manager.modules.finance.shared.service;

import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

@Service
public class DocumentNumberGeneratorService {

    public String nextPurchaseInvoiceNumber(
            UUID branchId
    ) {

        String datePart =
                LocalDate.now()
                        .format(
                                DateTimeFormatter.ofPattern(
                                        "yyyyMMdd"
                                )
                        );

        String random =
                UUID.randomUUID()
                        .toString()
                        .substring(0, 8)
                        .toUpperCase();

        return "PINV-"
                + datePart
                + "-"
                + random;
    }

    public String nextSupplierPaymentNumber(
            UUID branchId
    ) {

        String datePart =
                LocalDate.now()
                        .format(
                                DateTimeFormatter.ofPattern(
                                        "yyyyMMdd"
                                )
                        );

        String random =
                UUID.randomUUID()
                        .toString()
                        .substring(0, 8)
                        .toUpperCase();

        return "SPAY-"
                + datePart
                + "-"
                + random;
    }
}