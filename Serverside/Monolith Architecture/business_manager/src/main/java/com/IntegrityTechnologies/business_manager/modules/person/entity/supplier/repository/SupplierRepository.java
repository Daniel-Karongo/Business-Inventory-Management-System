package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
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
    Optional<Supplier> findByIdAndDeletedTrue(UUID id);
    Optional<Supplier> findById(UUID id);

    boolean existsByNameIgnoreCase(String name);

    // Search the element collection table for email
    @Query("select s from Supplier s join s.email e where lower(e) = lower(:email)")
    Optional<Supplier> findByEmailElementIgnoreCase(@Param("email") String email);
    @Query("select s from Supplier s join s.email e where lower(e) = lower(:email) and s.deleted = false")
    Optional<Supplier> findByEmailElementIgnoreCaseAndDeletedFalse(@Param("email") String email);
    @Query("select s from Supplier s join s.email e where lower(e) = lower(:email) and s.deleted = true")
    Optional<Supplier> findByEmailElementIgnoreCaseAndDeletedTrue(@Param("email") String email);

    // Search phone numbers (element collection)
    @Query("select s from Supplier s join s.phoneNumber p where p = :phone")
    Optional<Supplier> findByPhoneNumberElement(@Param("phone") String phone);
    @Query("select s from Supplier s join s.phoneNumber p where p = :phone and s.deleted = false")
    Optional<Supplier> findByPhoneNumberElementAndDeletedFalse(@Param("phone") String phone);
    @Query("select s from Supplier s join s.phoneNumber p where p = :phone and s.deleted = true")
    Optional<Supplier> findByPhoneNumberElementAndDeletedTrue(@Param("phone") String phone);

    // Search name case-insensitive
    Optional<Supplier> findByNameIgnoreCase(String name);
    Optional<Supplier> findByNameIgnoreCaseAndDeletedTrue(String name);
    Optional<Supplier> findByNameIgnoreCaseAndDeletedFalse(String name);

    // Fetch suppliers with categories, images, emails, phone numbers in one query
    List<Supplier> findAll();

    // Bulk detach suppliers from categories
    @Modifying
    @Query(value = "DELETE FROM supplier_categories WHERE supplier_id IN :supplierIds", nativeQuery = true)
    void detachFromCategoriesBulk(@Param("supplierIds") List<UUID> supplierIds);

    // Bulk detach suppliers from products
    @Modifying
    @Query(value = "DELETE FROM products_suppliers WHERE supplier_id IN :supplierIds", nativeQuery = true)
    void detachFromProductsBulk(@Param("supplierIds") List<UUID> supplierIds);

    // Bulk delete suppliers
    @Modifying
    @Query("DELETE FROM Supplier s WHERE s.id IN :supplierIds")
    void deleteSuppliersByIds(@Param("supplierIds") List<UUID> supplierIds);
}