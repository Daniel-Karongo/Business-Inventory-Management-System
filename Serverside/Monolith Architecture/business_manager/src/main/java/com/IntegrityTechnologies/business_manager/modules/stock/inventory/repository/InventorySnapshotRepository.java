package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventorySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface InventorySnapshotRepository extends JpaRepository<InventorySnapshot, UUID> {

    List<InventorySnapshot> findBySnapshotDate(LocalDate date);

    List<InventorySnapshot> findByProductVariantIdAndBranchIdOrderBySnapshotDateDesc(
            UUID productVariantId, UUID branchId);

    Optional<InventorySnapshot>
    findTopByProductVariantIdAndBranchIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
            UUID productVariantId,
            UUID branchId,
            LocalDate date
    );
}