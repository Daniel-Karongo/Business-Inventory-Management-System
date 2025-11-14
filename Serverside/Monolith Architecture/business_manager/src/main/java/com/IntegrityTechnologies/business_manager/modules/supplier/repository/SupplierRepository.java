package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, UUID>, JpaSpecificationExecutor<Supplier> {
    List<Supplier> findByDeletedFalse();

    List<Supplier> findByDeletedTrue();

    Optional<Supplier> findByIdAndDeletedFalse(UUID id);

    boolean existsByNameIgnoreCase(String name);

    // Search the element collection table for email
    @Query("select s from Supplier s join s.email e where lower(e) = lower(:email) and s.deleted = false")
    Optional<Supplier> findByEmailElementIgnoreCase(@Param("email") String email);

    // Search phone numbers (element collection)
    @Query("select s from Supplier s join s.phoneNumber p where p = :phone and s.deleted = false")
    Optional<Supplier> findByPhoneNumberElement(@Param("phone") String phone);

    // Search name case-insensitive
    Optional<Supplier> findByNameIgnoreCase(String name);
    Optional<Supplier> findByNameIgnoreCaseAndDeletedFalse(String name);
}