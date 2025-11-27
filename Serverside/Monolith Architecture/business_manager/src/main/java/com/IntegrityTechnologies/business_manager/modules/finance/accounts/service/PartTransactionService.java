package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;

import java.util.List;
import java.util.UUID;

public interface PartTransactionService {
    PartTransaction record(PartTransaction tx);
    List<PartTransaction> listByModuleAndRef(String module, UUID refId);
}