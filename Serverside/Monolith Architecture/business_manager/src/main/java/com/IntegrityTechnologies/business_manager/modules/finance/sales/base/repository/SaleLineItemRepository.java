package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface SaleLineItemRepository extends JpaRepository<SaleLineItem, Long> {
    void deleteAllByProductVariantIdIn(List<UUID> variantIds);

}
