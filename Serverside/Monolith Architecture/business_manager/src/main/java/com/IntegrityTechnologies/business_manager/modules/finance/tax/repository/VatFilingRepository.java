package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatFiling;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface VatFilingRepository extends JpaRepository<VatFiling, UUID> {
}