package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import org.springframework.data.jpa.repository.*;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, Long>, JpaSpecificationExecutor<Supplier> {
    List<Supplier> findByDeletedFalse();
    Optional<Supplier> findByIdAndDeletedFalse(Long id);
    Optional<Supplier> findByNameIgnoreCase(String name);
    Optional<Supplier> findByEmailIgnoreCase(String email);
    Optional<Supplier> findByPhoneNumber(String phoneNumber);
    boolean existsByNameIgnoreCase(String name);
}
