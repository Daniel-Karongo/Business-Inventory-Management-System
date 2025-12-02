package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerResponse;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerPaymentHistory;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerPaymentHistoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CustomerService {

    private final CustomerRepository customerRepository;
    private final CustomerPaymentHistoryRepository cphRepo; // add to constructor args

    public CustomerResponse createCustomer(CustomerRequest req) {
        Customer c = Customer.builder()
                .name(req.getName())
                .phone(req.getPhone())
                .email(req.getEmail())
                .address(req.getAddress())
                .notes(req.getNotes())
                .build();
        Customer saved = customerRepository.save(c);
        return toResponse(saved);
    }

    public Page<CustomerResponse> listCustomers(int page, int size, String q) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        Page<Customer> pageResult;
        if (q == null || q.isBlank()) {
            pageResult = customerRepository.findAll(pageable);
        } else {
            pageResult = customerRepository.findByNameContainingIgnoreCase(q, pageable);
        }
        return pageResult.map(this::toResponse);
    }

    public CustomerResponse getCustomer(UUID id) {
        Customer c = customerRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
        return toResponse(c);
    }

    @Transactional
    public CustomerResponse updateCustomer(UUID id, CustomerRequest req) {
        Customer c = customerRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
        c.setName(req.getName());
        c.setPhone(req.getPhone());
        c.setEmail(req.getEmail());
        c.setAddress(req.getAddress());
        c.setNotes(req.getNotes());
        Customer updated = customerRepository.save(c);
        return toResponse(updated);
    }

    @Transactional
    public void deleteCustomer(UUID id) {
        Customer c = customerRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
        customerRepository.delete(c);
    }

    public CustomerResponse findByPhone(String phone) {
        return customerRepository.findByPhone(phone).map(this::toResponse).orElse(null);
    }

    private CustomerResponse toResponse(Customer c) {
        return CustomerResponse.builder()
                .id(c.getId())
                .name(c.getName())
                .phone(c.getPhone())
                .email(c.getEmail())
                .address(c.getAddress())
                .notes(c.getNotes())
                .createdAt(c.getCreatedAt())
                .updatedAt(c.getUpdatedAt())
                .build();
    }



    @Transactional
    public void recordPayment(UUID customerId, UUID paymentId, BigDecimal amount, LocalDateTime timestamp) {
        // silent no-op if customer missing
        if (customerId == null) return;
        Customer c = customerRepository.findById(customerId).orElse(null);
        if (c == null) return;
        CustomerPaymentHistory hist = CustomerPaymentHistory.builder()
                .id(UUID.randomUUID())
                .customerId(customerId)
                .paymentId(paymentId)
                .amount(amount)
                .timestamp(timestamp != null ? timestamp : LocalDateTime.now())
                .note("Recorded payment for sale")
                .build();
        cphRepo.save(hist);
    }
}