package com.IntegrityTechnologies.business_manager.modules.person.customer.service;

import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.customer.dto.CustomerResponse;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerPaymentHistory;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Gender;
import com.IntegrityTechnologies.business_manager.modules.person.customer.repository.CustomerPaymentHistoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerType.COMPANY;

@Service
@RequiredArgsConstructor
public class CustomerService {

    private final CustomerRepository customerRepository;
    private final CustomerPaymentHistoryRepository cphRepo;
    private final SaleRepository saleRepository;
    private final SmsService smsService;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public UUID findOrCreateCustomer(
            UUID branchId,
            List<CustomerRequest> identifiers
    ) {

        branchTenantGuard.validate(branchId);

        if (identifiers == null || identifiers.isEmpty()) {
            return null;
        }

        for (CustomerRequest req : identifiers) {

            if (req == null) continue;

            if (req.getCustomerId() != null) {

                return customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                req.getCustomerId()
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found: " + req.getCustomerId()
                                )
                        )
                        .getId();
            }

            Set<String> phones = new HashSet<>();

            if (req.getPhone() != null) {
                phones.addAll(
                        PhoneAndEmailNormalizer.normalizePhones(
                                List.of(req.getPhone())
                        )
                );
            }

            if (req.getPhoneNumbers() != null) {
                phones.addAll(
                        PhoneAndEmailNormalizer.normalizePhones(
                                req.getPhoneNumbers()
                        )
                );
            }

            Set<String> emails = new HashSet<>();

            if (req.getEmail() != null) {
                emails.addAll(
                        PhoneAndEmailNormalizer.normalizeEmails(
                                List.of(req.getEmail())
                        )
                );
            }

            if (req.getEmailAddresses() != null) {
                emails.addAll(
                        PhoneAndEmailNormalizer.normalizeEmails(
                                req.getEmailAddresses()
                        )
                );
            }

            for (String p : phones) {

                var found =
                        customerRepository.findByPhoneNumberElement(
                                tenantId(),
                                branchId,
                                p
                        );

                if (found.isPresent()) {
                    return found.get().getId();
                }
            }

            for (String e : emails) {

                var found =
                        customerRepository.findByEmailElementIgnoreCase(
                                tenantId(),
                                branchId,
                                e
                        );

                if (found.isPresent()) {
                    return found.get().getId();
                }
            }
        }

        CustomerRequest best = identifiers.get(0);

        Set<String> phones = new HashSet<>();

        if (best.getPhone() != null) {
            phones.addAll(
                    PhoneAndEmailNormalizer.normalizePhones(
                            List.of(best.getPhone())
                    )
            );
        }

        if (best.getPhoneNumbers() != null) {
            phones.addAll(
                    PhoneAndEmailNormalizer.normalizePhones(
                            best.getPhoneNumbers()
                    )
            );
        }

        Set<String> emails = new HashSet<>();

        if (best.getEmail() != null) {
            emails.addAll(
                    PhoneAndEmailNormalizer.normalizeEmails(
                            List.of(best.getEmail())
                    )
            );
        }

        if (best.getEmailAddresses() != null) {
            emails.addAll(
                    PhoneAndEmailNormalizer.normalizeEmails(
                            best.getEmailAddresses()
                    )
            );
        }

        Customer created = Customer.builder()
                .tenantId(tenantId())
                .branchId(branchId)
                .name(
                        best.getName() != null
                                ? best.getName()
                                : "Walk-in Customer"
                )
                .phoneNumbers(new ArrayList<>(phones))
                .emailAddresses(new ArrayList<>(emails))
                .type(
                        best.getType() != null
                                ? best.getType()
                                : CustomerType.INDIVIDUAL
                )
                .gender(best.getGender())
                .address(best.getAddress())
                .notes(best.getNotes())
                .build();

        Customer saved = customerRepository.save(created);

        return saved.getId();
    }

    @Transactional
    public CustomerResponse createCustomer(
            UUID branchId,
            CustomerRequest req
    ) {

        branchTenantGuard.validate(branchId);

        Customer c = Customer.builder()
                .tenantId(tenantId())
                .branchId(branchId)
                .name(req.getName())
                .phoneNumbers(
                        PhoneAndEmailNormalizer
                                .normalizePhones(req.getPhoneNumbers())
                                .stream()
                                .toList()
                )
                .emailAddresses(
                        PhoneAndEmailNormalizer
                                .normalizeEmails(req.getEmailAddresses())
                                .stream()
                                .toList()
                )
                .type(
                        req.getType() != null
                                ? req.getType()
                                : CustomerType.INDIVIDUAL
                )
                .gender(
                        req.getType() == COMPANY
                                ? null
                                : req.getGender()
                )
                .address(req.getAddress())
                .notes(req.getNotes())
                .build();

        Customer saved = customerRepository.save(c);

        return toResponse(saved);
    }

    @Transactional(readOnly = true)
    public Page<CustomerResponse> listCustomers(
            UUID branchId,
            int page,
            int size,
            CustomerType type,
            Gender gender,
            Boolean deleted
    ) {

        branchTenantGuard.validate(branchId);

        Pageable p =
                PageRequest.of(
                        page,
                        size,
                        Sort.by("createdAt").descending()
                );

        Page<Customer> res;

        if (type != null && gender != null && deleted != null) {

            res =
                    customerRepository
                            .findByTenantIdAndBranchIdAndTypeAndGenderAndDeleted(
                                    tenantId(),
                                    branchId,
                                    type,
                                    gender,
                                    deleted,
                                    p
                            );

        } else if (type != null && deleted != null) {

            res =
                    customerRepository
                            .findByTenantIdAndBranchIdAndTypeAndDeleted(
                                    tenantId(),
                                    branchId,
                                    type,
                                    deleted,
                                    p
                            );

        } else if (deleted != null) {

            res =
                    customerRepository
                            .findByTenantIdAndBranchIdAndDeleted(
                                    tenantId(),
                                    branchId,
                                    deleted,
                                    p
                            );

        } else {

            res =
                    customerRepository
                            .findByTenantIdAndBranchIdAndDeletedFalse(
                                    tenantId(),
                                    branchId,
                                    p
                            );
        }

        return res.map(this::toResponse);
    }

    @Transactional(readOnly = true)
    public CustomerResponse getCustomer(
            UUID branchId,
            UUID id
    ) {

        branchTenantGuard.validate(branchId);

        Customer c =
                customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                id
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found: " + id
                                )
                        );

        return toResponse(c);
    }

    @Transactional(readOnly = true)
    public Optional<CustomerResponse> findByPhone(
            UUID branchId,
            String phone
    ) {

        branchTenantGuard.validate(branchId);

        var normalized =
                PhoneAndEmailNormalizer.normalizePhones(
                        List.of(phone)
                );

        for (String p : normalized) {

            var found =
                    customerRepository.findByPhoneNumberElement(
                            tenantId(),
                            branchId,
                            p
                    );

            if (found.isPresent()) {
                return Optional.of(toResponse(found.get()));
            }
        }

        return Optional.empty();
    }

    @Transactional
    public CustomerResponse updateCustomer(
            UUID branchId,
            UUID id,
            CustomerRequest req
    ) {

        branchTenantGuard.validate(branchId);

        Customer c =
                customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                id
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found: " + id
                                )
                        );

        c.setName(req.getName());

        c.setPhoneNumbers(
                PhoneAndEmailNormalizer
                        .normalizePhones(req.getPhoneNumbers())
                        .stream()
                        .toList()
        );

        c.setEmailAddresses(
                PhoneAndEmailNormalizer
                        .normalizeEmails(req.getEmailAddresses())
                        .stream()
                        .toList()
        );

        c.setType(req.getType());

        c.setGender(
                req.getType() == COMPANY
                        ? null
                        : req.getGender()
        );

        c.setAddress(req.getAddress());
        c.setNotes(req.getNotes());

        Customer updated =
                customerRepository.save(c);

        return toResponse(updated);
    }

    @Transactional
    public void deleteCustomer(
            UUID branchId,
            UUID id
    ) {

        branchTenantGuard.validate(branchId);

        Customer c =
                customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                id
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found: " + id
                                )
                        );

        customerRepository.delete(c);
    }

    @Transactional
    public void softDelete(
            UUID branchId,
            UUID id
    ) {

        branchTenantGuard.validate(branchId);

        Customer c =
                customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                id
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found"
                                )
                        );

        c.setDeleted(true);

        customerRepository.save(c);
    }

    @Transactional
    public void restore(
            UUID branchId,
            UUID id
    ) {

        branchTenantGuard.validate(branchId);

        Customer c =
                customerRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                id
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Customer not found"
                                )
                        );

        c.setDeleted(false);

        customerRepository.save(c);
    }

    @Transactional(readOnly = true)
    public Page<CustomerResponse> search(
            UUID branchId,
            String q,
            int page,
            int size
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable =
                PageRequest.of(
                        page,
                        size,
                        Sort.by(
                                Sort.Direction.DESC,
                                "createdAt"
                        )
                );

        return customerRepository
                .findByTenantIdAndBranchIdAndNameContainingIgnoreCase(
                        tenantId(),
                        branchId,
                        q,
                        pageable
                )
                .map(this::toResponse);
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> getCustomerPayments(
            UUID branchId,
            UUID customerId
    ) {

        branchTenantGuard.validate(branchId);

        var list =
                cphRepo.findByTenantIdAndBranchIdAndCustomerId(
                        tenantId(),
                        branchId,
                        customerId
                );

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
    public List<Map<String, Object>> getCustomerSales(
            UUID branchId,
            UUID customerId
    ) {

        branchTenantGuard.validate(branchId);

        List<Sale> sales =
                saleRepository.findByCustomerId(customerId);

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
                .gender(c.getGender())
                .deleted(c.getDeleted())
                .type(c.getType())
                .address(c.getAddress())
                .notes(c.getNotes())
                .createdAt(c.getCreatedAt())
                .updatedAt(c.getUpdatedAt())
                .build();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void recordPayment(
            UUID branchId,
            UUID customerId,
            UUID paymentId,
            BigDecimal amount,
            LocalDateTime timestamp
    ) {

        branchTenantGuard.validate(branchId);

        if (customerId == null) return;

        CustomerPaymentHistory hist =
                CustomerPaymentHistory.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .customerId(customerId)
                        .paymentId(paymentId)
                        .amount(amount)
                        .timestamp(
                                timestamp != null
                                        ? timestamp
                                        : LocalDateTime.now()
                        )
                        .note("Recorded payment for sale")
                        .build();

        cphRepo.save(hist);
    }

    @Transactional
    public void recordRefund(
            UUID branchId,
            UUID customerId,
            UUID saleId,
            BigDecimal amount,
            String note
    ) {

        branchTenantGuard.validate(branchId);

        if (customerId == null) return;

        CustomerPaymentHistory h =
                CustomerPaymentHistory.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .customerId(customerId)
                        .paymentId(saleId)
                        .amount(amount.negate())
                        .timestamp(LocalDateTime.now())
                        .note(
                                note != null
                                        ? note
                                        : "Refund applied"
                        )
                        .build();

        cphRepo.save(h);
    }

    public Map<String, Object> sendToCustomers(
            UUID branchId,
            List<UUID> customerIds,
            String message
    ) {

        branchTenantGuard.validate(branchId);

        List<Customer> customers =
                customerIds.stream()
                        .map(id ->
                                customerRepository
                                        .findByTenantIdAndBranchIdAndId(
                                                tenantId(),
                                                branchId,
                                                id
                                        )
                                        .orElseThrow(() ->
                                                new EntityNotFoundException(
                                                        "Customer not found: " + id
                                                )
                                        )
                        )
                        .toList();

        Set<String> phones =
                customers.stream()
                        .flatMap(c -> c.getPhoneNumbers().stream())
                        .collect(Collectors.toSet());

        int skipped =
                customers.size() - phones.size();

        List<SmsRequest> requests =
                phones.stream()
                        .map(p -> {
                            SmsRequest r = new SmsRequest();
                            r.setToPhone(p);
                            r.setMessage(message);
                            return r;
                        })
                        .toList();

        List<SmsMessage> sent =
                smsService.sendBulk(
                        branchId,
                        requests
                );

        return Map.of(
                "sent", sent.size(),
                "skipped", skipped
        );
    }

    public byte[] exportCsv(
            UUID branchId,
            String q,
            String type,
            String gender,
            Boolean deleted
    ) {

        branchTenantGuard.validate(branchId);

        List<Customer> customers =
                customerRepository.searchAdvanced(
                        tenantId(),
                        branchId,
                        q,
                        type,
                        gender,
                        deleted
                );

        StringBuilder sb = new StringBuilder();

        sb.append("Name,Type,Gender,Phones,Emails\n");

        for (Customer c : customers) {

            sb.append(c.getName()).append(",");
            sb.append(c.getType()).append(",");
            sb.append(c.getGender()).append(",");
            sb.append(String.join(" | ", c.getPhoneNumbers())).append(",");
            sb.append(String.join(" | ", c.getEmailAddresses())).append("\n");
        }

        return sb.toString()
                .getBytes(StandardCharsets.UTF_8);
    }
}