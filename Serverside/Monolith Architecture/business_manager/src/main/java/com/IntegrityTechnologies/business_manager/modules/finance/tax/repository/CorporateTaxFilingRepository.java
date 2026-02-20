package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface CorporateTaxFilingRepository extends JpaRepository<CorporateTaxFiling, UUID> {
}