package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.StockTransactionDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockTransactionService {

    private final StockTransactionRepository stockRepository;

    public List<StockTransactionDTO> getByProduct(UUID productId) {
        return stockRepository.findByProductIdOrderByTimestampDesc(productId)
                .stream().map(this::toDTO)
                .toList();
    }

    public List<StockTransactionDTO> getByVariant(UUID variantId) {
        return stockRepository.findByProductVariantIdOrderByTimestampDesc(variantId)
                .stream().map(this::toDTO)
                .toList();
    }

    public List<StockTransactionDTO> getByBranch(UUID branchId) {
        return stockRepository.findByBranchIdOrderByTimestampDesc(branchId)
                .stream().map(this::toDTO)
                .toList();
    }

    public List<StockTransactionDTO> getByProductAndBranch(UUID productId, UUID branchId) {
        return stockRepository.findByProductIdAndBranchIdOrderByTimestampDesc(productId, branchId)
                .stream().map(this::toDTO)
                .toList();
    }

    public List<StockTransactionDTO> getByVariantAndBranch(UUID variantId, UUID branchId) {
        return stockRepository.findByProductVariantIdAndBranchIdOrderByTimestampDesc(variantId, branchId)
                .stream().map(this::toDTO)
                .toList();
    }

    public List<StockTransactionDTO> getByDateRange(LocalDate from, LocalDate to) {
        return stockRepository.findByTimestampBetween(
                        from.atStartOfDay(),
                        to.atTime(23, 59, 59))
                .stream().map(this::toDTO)
                .toList();
    }

    private StockTransactionDTO toDTO(StockTransaction t) {
        return StockTransactionDTO.builder()
                .id(t.getId())
                .productId(t.getProductId())
                .productVariantId(t.getProductVariantId())
                .branchId(t.getBranchId())
                .type(t.getType().name())
                .quantityDelta(t.getQuantityDelta())
                .unitCost(t.getUnitCost())
                .reference(t.getReference())
                .supplierId(t.getSupplierId())
                .note(t.getNote())
                .timestamp(t.getTimestamp())
                .performedBy(t.getPerformedBy())
                .build();
    }
}