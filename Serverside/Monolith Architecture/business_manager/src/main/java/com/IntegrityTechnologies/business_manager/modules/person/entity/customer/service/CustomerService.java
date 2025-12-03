package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CustomerService {

    private final CustomerRepository customerRepository;
    private final CustomerPaymentHistoryRepository cphRepo; // add to constructor args
    private final SaleRepository saleRepository;

    @Transactional
    public UUID findOrCreateCustomer(List<CustomerRequest> identifiers) {

        if (identifiers == null || identifiers.isEmpty()) {
            return null; // sale may not have a customer
        }

        for (CustomerRequest req : identifiers) {

            // 1️⃣ If customerId provided → load that customer first
            if (req.getCustomerId() != null) {
                return customerRepository.findById(req.getCustomerId())
                        .orElseThrow(() -> new EntityNotFoundException("Customer not found: " + req.getCustomerId()))
                        .getId();
            }

            // Prepare normalized lists
            List<String> phones = req.getPhoneNumbers() != null
                    ? PhoneAndEmailNormalizer.normalizePhones(req.getPhoneNumbers()).stream().toList()
                    : List.of();

            List<String> emails = req.getEmailAddresses() != null
                    ? PhoneAndEmailNormalizer.normalizeEmails(req.getEmailAddresses()).stream().toList()
                    : List.of();

            // 2️⃣ Match by phone numbers
            for (String p : phones) {
                var found = customerRepository.findByPhoneNumberElement(p);
                if (found.isPresent()) return found.get().getId();
            }

            // 3️⃣ Match by email
            for (String e : emails) {
                var found = customerRepository.findByEmailElementIgnoreCase(e);
                if (found.isPresent()) return found.get().getId();
            }

            // 4️⃣ Match by exact name
            if (req.getName() != null) {
                var found = customerRepository.findByNameIgnoreCase(req.getName());
                if (found.isPresent()) return found.get().getId();
            }
        }

        // 5️⃣ If none matched, create a new customer
        CustomerRequest best = identifiers.get(0); // take the first
        Customer created = Customer.builder()
                .name(best.getName() != null ? best.getName() : "Walk-in Customer")
                .phoneNumbers(
                        best.getPhoneNumbers() != null ?
                                PhoneAndEmailNormalizer.normalizePhones(best.getPhoneNumbers()).stream().toList() : List.of()
                )
                .emailAddresses(
                        best.getEmailAddresses() != null ?
                                PhoneAndEmailNormalizer.normalizeEmails(best.getEmailAddresses()).stream().toList() : List.of()
                )
                .address(best.getAddress())
                .notes(best.getNotes())
                .build();

        Customer saved = customerRepository.save(created);
        return saved.getId();
    }

    public CustomerResponse createCustomer(CustomerRequest req) {
        Customer c = Customer.builder()
                .name(req.getName())
                .phoneNumbers(PhoneAndEmailNormalizer.normalizePhones(req.getPhoneNumbers()).stream().toList())
                .emailAddresses(PhoneAndEmailNormalizer.normalizeEmails(req.getEmailAddresses()).stream().toList())
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
        c.setPhoneNumbers(PhoneAndEmailNormalizer.normalizePhones(req.getPhoneNumbers()).stream().toList());
        c.setEmailAddresses(PhoneAndEmailNormalizer.normalizeEmails(req.getEmailAddresses()).stream().toList());
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

    @Transactional(readOnly = true)
    public Page<CustomerResponse> search(String q, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        return customerRepository.findByNameContainingIgnoreCase(q, pageable).map(this::toResponse);
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> getCustomerPayments(UUID customerId) {
        // assume CustomerPaymentHistory entity and repo exists
        var list = cphRepo.findByCustomerId(customerId);
        return list.stream().map(h -> {
            Map<String, Object> m = new HashMap<>();
            m.put("paymentId", h.getPaymentId());
            m.put("amount", h.getAmount());
            m.put("timestamp", h.getTimestamp());
            m.put("note", h.getNote());
            return m;
        }).toList();
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> getCustomerSales(UUID customerId) {
        List<Sale> sales = saleRepository.findByCustomerId(customerId);
        return sales.stream().map(s -> {
            Map<String, Object> m = new HashMap<>();
            m.put("saleId", s.getId());
            m.put("totalAmount", s.getTotalAmount());
            m.put("createdAt", s.getCreatedAt());
            m.put("status", s.getStatus());
            return m;
        }).toList();
    }

    private CustomerResponse toResponse(Customer c) {
        return CustomerResponse.builder()
                .id(c.getId())
                .name(c.getName())
                .phoneNumbers(c.getPhoneNumbers())
                .email(c.getEmailAddresses())
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