package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;

@Service
@RequiredArgsConstructor
public class BatchProfitService {

    private final BatchConsumptionRepository batchRepo;
    private final SaleRepository saleRepository;

    public List<Map<String, Object>> calculateBatchProfit(UUID batchId) {

        List<BatchConsumption> consumptions =
                batchRepo.findByBatchId(batchId);

        BigDecimal totalCost = BigDecimal.ZERO;
        BigDecimal totalRevenue = BigDecimal.ZERO;
        Long totalQty = 0L;

        for (BatchConsumption c : consumptions) {

            totalCost = totalCost.add(
                    c.getUnitCost().multiply(BigDecimal.valueOf(c.getQuantity()))
            );

            var sale = saleRepository.findById(c.getSaleId()).orElse(null);

            if (sale != null) {

                BigDecimal saleRevenue = sale.getLineItems().stream()
                        .filter(li -> li.getProductVariantId().equals(c.getProductVariantId()))
                        .map(li -> li.getNetAmount())
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

                totalRevenue = totalRevenue.add(saleRevenue);
            }

            totalQty += c.getQuantity();
        }

        BigDecimal profit = totalRevenue.subtract(totalCost);

        return List.of(
                Map.of(
                        "batchId", batchId,
                        "quantitySold", totalQty,
                        "revenue", totalRevenue,
                        "cost", totalCost,
                        "profit", profit
                )
        );
    }
}