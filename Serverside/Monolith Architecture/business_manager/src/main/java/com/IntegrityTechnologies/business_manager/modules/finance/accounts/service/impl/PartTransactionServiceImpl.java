package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.PartTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.PartTransactionService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PartTransactionServiceImpl implements PartTransactionService {

    private final PartTransactionRepository repo;

    @Override
    @Transactional
    public PartTransaction record(PartTransaction tx) {
        if (tx.getTransactionDate() == null) tx.setTransactionDate(LocalDateTime.now());
        return repo.save(tx);
    }

    @Override
    public List<PartTransaction> listByModuleAndRef(String module, UUID refId) {
        return repo.findByRelatedModuleAndReferenceId(module, refId);
    }
}