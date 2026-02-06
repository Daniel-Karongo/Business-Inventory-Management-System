package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Gender;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface CustomerRepository extends JpaRepository<Customer, UUID> {

    // Match ANY phone number in the phoneNumbers collection
    @Query("SELECT c FROM Customer c JOIN c.phoneNumbers p WHERE p = :phone")
    Optional<Customer> findByPhoneNumberElement(@Param("phone") String phone);

    // Match ANY email in the emailAddresses collection
    @Query("SELECT c FROM Customer c JOIN c.emailAddresses e WHERE LOWER(e) = LOWER(:email)")
    Optional<Customer> findByEmailElementIgnoreCase(@Param("email") String email);

    // Name match
    Optional<Customer> findByNameIgnoreCase(String name);

    // Already existing from your original code
    Page<Customer> findByNameContainingIgnoreCase(String q, Pageable pageable);
    Page<Customer> findByTypeAndGender(CustomerType type, Gender gender, Pageable p);
    Page<Customer> findByType(CustomerType type, Pageable p);
    Page<Customer> findByDeletedFalse(Pageable p);
    Page<Customer> findByDeleted(Boolean deleted, Pageable p);
    Page<Customer> findByTypeAndDeleted(CustomerType type, Boolean deleted, Pageable p);
    Page<Customer> findByTypeAndGenderAndDeleted(
            CustomerType type,
            Gender gender,
            Boolean deleted,
            Pageable p
    );

    @Query("""
    SELECT DISTINCT c
    FROM Customer c
    WHERE
        (:q IS NULL OR
            LOWER(c.name) LIKE LOWER(CONCAT('%', :q, '%')) OR
            EXISTS (
                SELECT p FROM c.phoneNumbers p
                WHERE p LIKE CONCAT('%', :q, '%')
            ) OR
            EXISTS (
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
            @Param("q") String q,
            @Param("type") String type,
            @Param("gender") String gender,
            @Param("deleted") Boolean deleted
    );

    boolean existsByPhoneNumbersContains(String phone);

    boolean existsByEmailAddressesContains(String email);
}
