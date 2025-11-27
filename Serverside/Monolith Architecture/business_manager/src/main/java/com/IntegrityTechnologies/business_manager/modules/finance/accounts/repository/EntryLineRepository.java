package com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.EntryLine;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface EntryLineRepository extends JpaRepository<EntryLine, UUID> {
}