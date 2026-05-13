package com.IntegrityTechnologies.business_manager.modules.person.customer.repository;

import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Gender;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface CustomerRepository
        extends JpaRepository<Customer, UUID> {

    @Query("""
                SELECT c
                FROM Customer c
                JOIN c.phoneNumbers p
                WHERE c.tenantId = :tenantId
                AND c.branchId = :branchId
                AND p = :phone
            """)
    Optional<Customer> findByPhoneNumberElement(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("phone") String phone
    );

    @Query("""
                SELECT c
                FROM Customer c
                JOIN c.emailAddresses e
                WHERE c.tenantId = :tenantId
                AND c.branchId = :branchId
                AND LOWER(e) = LOWER(:email)
            """)
    Optional<Customer> findByEmailElementIgnoreCase(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("email") String email
    );

    Optional<Customer>
    findByTenantIdAndBranchIdAndNameIgnoreCase(
            UUID tenantId,
            UUID branchId,
            String name
    );

    Optional<Customer>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndNameContainingIgnoreCase(
            UUID tenantId,
            UUID branchId,
            String q,
            Pageable pageable
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndTypeAndGender(
            UUID tenantId,
            UUID branchId,
            CustomerType type,
            Gender gender,
            Pageable p
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndType(
            UUID tenantId,
            UUID branchId,
            CustomerType type,
            Pageable p
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndDeletedFalse(
            UUID tenantId,
            UUID branchId,
            Pageable p
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndDeleted(
            UUID tenantId,
            UUID branchId,
            Boolean deleted,
            Pageable p
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndTypeAndDeleted(
            UUID tenantId,
            UUID branchId,
            CustomerType type,
            Boolean deleted,
            Pageable p
    );

    Page<Customer>
    findByTenantIdAndBranchIdAndTypeAndGenderAndDeleted(
            UUID tenantId,
            UUID branchId,
            CustomerType type,
            Gender gender,
            Boolean deleted,
            Pageable p
    );

    @Query("""
                SELECT DISTINCT c
                FROM Customer c
                WHERE c.tenantId = :tenantId
                AND c.branchId = :branchId
                AND (
                    :q IS NULL OR
                    LOWER(c.name) LIKE LOWER(CONCAT('%', :q, '%'))
                    OR EXISTS (
                        SELECT p FROM c.phoneNumbers p
                        WHERE p LIKE CONCAT('%', :q, '%')
                    )
                    OR EXISTS (
                        SELECT e FROM c.emailAddresses e
                        WHERE LOWER(e) LIKE LOWER(CONCAT('%', :q, '%'))
                    )
                )
                AND (:type IS NULL OR c.type = :type)
                AND (:gender IS NULL OR c.gender = :gender)
                AND (:deleted IS NULL OR c.deleted = :deleted)
                ORDER BY c.createdAt DESC
            """)
    List<Customer> searchAdvanced(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("q") String q,
            @Param("type") String type,
            @Param("gender") String gender,
            @Param("deleted") Boolean deleted
    );

    boolean existsByTenantIdAndBranchIdAndPhoneNumbersContains(
            UUID tenantId,
            UUID branchId,
            String phone
    );

    boolean existsByTenantIdAndBranchIdAndEmailAddressesContains(
            UUID tenantId,
            UUID branchId,
            String email
    );
}