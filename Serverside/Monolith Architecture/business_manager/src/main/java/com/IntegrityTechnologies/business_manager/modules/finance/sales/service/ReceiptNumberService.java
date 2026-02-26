package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.ReceiptSequence;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.ReceiptSequenceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class ReceiptNumberService {

    private static final long INITIAL_VALUE = 1_000_000_000L;

    private final ReceiptSequenceRepository repository;
    private final SaleRepository saleRepository;

    @Transactional
    public String nextSaleReceipt() {

        ReceiptSequence seq =
                repository.findById("SALE")
                        .orElseGet(this::initializeSequence);

        // 🔐 LOCK
        seq = repository.lockByName("SALE");

        // 🔥 SELF-HEALING SYNC
        Long maxExisting = saleRepository.findMaxReceiptNumeric();
        long dbMax = maxExisting != null ? maxExisting : INITIAL_VALUE;

        if (seq.getNextValue() <= dbMax) {
            seq.setNextValue(dbMax + 1);
        }

        long value = seq.getNextValue();
        seq.setNextValue(value + 1);

        return "R-" + value;
    }

    private ReceiptSequence initializeSequence() {
        ReceiptSequence seq = new ReceiptSequence();
        seq.setName("SALE");
        seq.setNextValue(INITIAL_VALUE);
        return repository.save(seq);
    }
}