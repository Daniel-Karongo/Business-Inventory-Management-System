package com.IntegrityTechnologies.business_manager.modules.sales.repository;

import com.IntegrityTechnologies.business_manager.modules.sales.model.Sale;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface SaleRepository extends JpaRepository<Sale, UUID> {
}