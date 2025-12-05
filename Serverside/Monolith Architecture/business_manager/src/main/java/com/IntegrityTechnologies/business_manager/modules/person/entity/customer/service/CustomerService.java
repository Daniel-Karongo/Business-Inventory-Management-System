package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerResponse;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerPaymentHistory;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerPaymentHistoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class CustomerService {

    private final CustomerRepository customerRepository;
    private final CustomerPaymentHistoryRepository cphRepo;
    private final SaleRepository saleRepository;

    @Transactional
    public UUID findOrCreateCustomer(List<CustomerRequest> identifiers) {

        if (identifiers == null || identifiers.isEmpty()) {
            return null;
        }

        for (CustomerRequest req : identifiers) {
            if (req == null) continue;

            if (req.getCustomerId() != null) {
                return customerRepository.findById(req.getCustomerId())
                        .orElseThrow(() -> new EntityNotFoundException("Customer not found: " + req.getCustomerId()))
                        .getId();
            }

            // normalize phones + emails
            Set<String> phones = new HashSet<>();
            if (req.getPhone() != null) phones.addAll(PhoneAndEmailNormalizer.normalizePhones(List.of(req.getPhone())));
            if (req.getPhoneNumbers() != null) phones.addAll(PhoneAndEmailNormalizer.normalizePhones(req.getPhoneNumbers()));

            Set<String> emails = new HashSet<>();
            if (req.getEmail() != null) emails.addAll(PhoneAndEmailNormalizer.normalizeEmails(List.of(req.getEmail())));
            if (req.getEmailAddresses() != null) emails.addAll(PhoneAndEmailNormalizer.normalizeEmails(req.getEmailAddresses()));

            // match by phone
            for (String p : phones) {
                var found = customerRepository.findByPhoneNumberElement(p);
                if (found.isPresent()) return found.get().getId();
            }

            // match by email
            for (String e : emails) {
                var found = customerRepository.findByEmailElementIgnoreCase(e);
                if (found.isPresent()) return found.get().getId();
            }

            // match by name
            if (req.getName() != null) {
                var found = customerRepository.findByNameIgnoreCase(req.getName());
                if (found.isPresent()) return found.get().getId();
            }
        }

        // create a new customer using the first identifier
        CustomerRequest best = identifiers.get(0);
        Set<String> phones = new HashSet<>();
        if (best.getPhone() != null) phones.addAll(PhoneAndEmailNormalizer.normalizePhones(List.of(best.getPhone())));
        if (best.getPhoneNumbers() != null) phones.addAll(PhoneAndEmailNormalizer.normalizePhones(best.getPhoneNumbers()));

        Set<String> emails = new HashSet<>();
        if (best.getEmail() != null) emails.addAll(PhoneAndEmailNormalizer.normalizeEmails(List.of(best.getEmail())));
        if (best.getEmailAddresses() != null) emails.addAll(PhoneAndEmailNormalizer.normalizeEmails(best.getEmailAddresses()));

        Customer created = Customer.builder()
                .name(best.getName() != null ? best.getName() : "Walk-in Customer")
                .phoneNumbers(new ArrayList<>(phones))
                .emailAddresses(new ArrayList<>(emails))
                .address(best.getAddress())
                .notes(best.getNotes())
                .createdAt(LocalDateTime.now())
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
        Customer c = customerRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
        return toResponse(c);
    }

    @Transactional
    public CustomerResponse updateCustomer(UUID id, CustomerRequest req) {
        Customer c = customerRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
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
        Customer c = customerRepository.findById(id).orElseThrow(() -> new EntityNotFoundException("Customer not found: " + id));
        customerRepository.delete(c);
    }

    @Transactional(readOnly = true)
    public Page<CustomerResponse> search(String q, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        return customerRepository.findByNameContainingIgnoreCase(q, pageable).map(this::toResponse);
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> getCustomerPayments(UUID customerId) {
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

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void recordPayment(UUID customerId, UUID paymentId, BigDecimal amount, LocalDateTime timestamp) {
        if (customerId == null) return;
        CustomerPaymentHistory hist = CustomerPaymentHistory.builder()
                .customerId(customerId)
                .paymentId(paymentId)
                .amount(amount)
                .timestamp(timestamp != null ? timestamp : LocalDateTime.now())
                .note("Recorded payment for sale")
                .build();
        cphRepo.save(hist);
    }

    @Transactional
    public void recordRefund(UUID customerId, UUID saleId, BigDecimal amount, String note) {
        if (customerId == null) return;
        CustomerPaymentHistory h = CustomerPaymentHistory.builder()
                .customerId(customerId)
                .paymentId(saleId)
                .amount(amount.negate())
                .timestamp(LocalDateTime.now())
                .note(note != null ? note : "Refund applied")
                .build();
        cphRepo.save(h);
    }
}