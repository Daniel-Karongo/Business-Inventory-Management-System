package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.StockTransactionDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockTransactionService {

    private final StockTransactionRepository stockRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<StockTransactionDTO> getAll(UUID branchId) {
        return stockRepository.findAllDTO(tenantId(), branchId);
    }

    public List<StockTransactionDTO> getByVariant(UUID variantId, UUID branchId) {
        return stockRepository.findByVariantDTO(tenantId(), branchId, variantId);
    }

    public List<StockTransactionDTO> getByProduct(UUID productId, UUID branchId) {
        return stockRepository.findByProductDTO(tenantId(), branchId, productId);
    }
}