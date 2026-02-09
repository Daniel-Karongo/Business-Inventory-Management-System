package com.IntegrityTechnologies.business_manager.modules.finance.sales.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.ReceiptSequence;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

public interface ReceiptSequenceRepository
        extends JpaRepository<ReceiptSequence, String> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select r from ReceiptSequence r where r.name = :name")
    ReceiptSequence lockByName(@Param("name") String name);
}