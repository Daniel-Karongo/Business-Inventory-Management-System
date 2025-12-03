package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

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
}
