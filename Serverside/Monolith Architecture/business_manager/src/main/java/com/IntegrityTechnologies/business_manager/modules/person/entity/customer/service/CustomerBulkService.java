package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional
public class CustomerBulkService {

    private final CustomerService customerService;
    private final CustomerRepository customerRepository;

    public BulkResult<CustomerResponse> importCustomers(
            BulkRequest<CustomerBulkRow> request
    ) {

        BulkResult<CustomerResponse> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        /* =========================
           IN-FILE DUP TRACKING
           ========================= */
        Set<String> seenPhones = new HashSet<>();
        Set<String> seenEmails = new HashSet<>();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            CustomerBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                /* =========================
                   NORMALIZATION
                   ========================= */
                String name = toTitleCase(row.getName());

                List<String> phones =
                        normalizeMulti(
                                row.getPhoneNumbers(),
                                row.getPhone(),
                                false
                        );

                List<String> emails =
                        normalizeMulti(
                                row.getEmailAddresses(),
                                row.getEmail(),
                                true
                        );

                /* =========================
                   IN-FILE DUPLICATES
                   ========================= */
                for (String p : phones) {
                    if (!seenPhones.add(p)) {
                        throw new IllegalArgumentException(
                                "Duplicate phone number in file: " + p
                        );
                    }
                }

                for (String e : emails) {
                    if (!seenEmails.add(e)) {
                        throw new IllegalArgumentException(
                                "Duplicate email in file: " + e
                        );
                    }
                }

                /* =========================
                   DB DUPLICATES
                   ========================= */
                if (options.isSkipDuplicates()) {

                    for (String p : phones) {
                        if (customerRepository.existsByPhoneNumbersContains(p)) {
                            throw new IllegalArgumentException(
                                    "Phone number already exists: " + p
                            );
                        }
                    }

                    for (String e : emails) {
                        if (customerRepository.existsByEmailAddressesContains(e)) {
                            throw new IllegalArgumentException(
                                    "Email already exists: " + e
                            );
                        }
                    }
                }

                /* =========================
                   BUILD REQUEST
                   ========================= */
                CustomerRequest req = new CustomerRequest();
                req.setName(name);
                req.setPhoneNumbers(phones);
                req.setEmailAddresses(emails);
                req.setType(row.getType());
                req.setGender(
                        row.getType() == CustomerType.COMPANY
                                ? null
                                : row.getGender()
                );
                req.setAddress(row.getAddress());
                req.setNotes(row.getNotes());

                if (options.isDryRun()) {
                    result.addSuccess(
                            CustomerResponse.builder()
                                    .name(req.getName())
                                    .phoneNumbers(req.getPhoneNumbers())
                                    .email(req.getEmailAddresses())
                                    .type(req.getType())
                                    .gender(req.getGender())
                                    .address(req.getAddress())
                                    .notes(req.getNotes())
                                    .build()
                    );
                } else {
                    CustomerResponse saved =
                            customerService.createCustomer(req);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        /* =========================
           HARD ROLLBACK
           ========================= */
        if (options.isDryRun()) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();
        }

        return result;
    }

    /* =========================
       VALIDATION
       ========================= */

    private void validate(CustomerBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getType() == null)
            throw new IllegalArgumentException("type is required");

        if (row.getType() == CustomerType.INDIVIDUAL && row.getGender() == null)
            throw new IllegalArgumentException("gender is required for INDIVIDUAL");
    }

    /* =========================
       HELPERS
       ========================= */

    private List<String> normalizeMulti(
            List<String> list,
            String single,
            boolean lowercase
    ) {
        Set<String> out = new LinkedHashSet<>();

        if (list != null) {
            list.forEach(v -> splitAndAdd(out, v, lowercase));
        }

        if (single != null) {
            splitAndAdd(out, single, lowercase);
        }

        return out.stream().toList();
    }

    private void splitAndAdd(Set<String> out, String raw, boolean lowercase) {
        if (raw == null) return;

        Arrays.stream(raw.split(","))
                .map(String::trim)
                .filter(s -> !s.isBlank())
                .map(s -> lowercase ? s.toLowerCase() : s)
                .forEach(out::add);
    }

    private String toTitleCase(String input) {
        String[] parts = input.trim().toLowerCase().split("\\s+");
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            sb.append(Character.toUpperCase(p.charAt(0)))
                    .append(p.substring(1))
                    .append(" ");
        }
        return sb.toString().trim();
    }
}