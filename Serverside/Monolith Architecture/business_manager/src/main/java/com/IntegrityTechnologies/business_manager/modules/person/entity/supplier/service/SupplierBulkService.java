package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class SupplierBulkService {

    private final SupplierService supplierService;
    private final CategoryRepository categoryRepository;

    public BulkResult<SupplierDTO> importSuppliers(
            BulkRequest<SupplierBulkRow> request,
            String creatorUsername
    ) {

        BulkResult<SupplierDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        /* =========================
           IN-FILE DUP TRACKING
           ========================= */
        Set<String> seenNames = new HashSet<>();
        Set<String> seenEmails = new HashSet<>();
        Set<String> seenPhones = new HashSet<>();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            SupplierBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                /* =========================
                   NORMALIZATION
                   ========================= */
                String name = toTitleCase(row.getName());

                Set<String> emails = normalizeSet(row.getEmail(), true);
                Set<String> phones = normalizeSet(row.getPhoneNumber(), false);

                /* =========================
                   IN-FILE DUPLICATES
                   ========================= */
                if (!seenNames.add(name.toLowerCase())) {
                    throw new IllegalArgumentException(
                            "Duplicate supplier name in file: " + name
                    );
                }

                for (String e : emails) {
                    if (!seenEmails.add(e)) {
                        throw new IllegalArgumentException(
                                "Duplicate email in file: " + e
                        );
                    }
                }

                for (String p : phones) {
                    if (!seenPhones.add(p)) {
                        throw new IllegalArgumentException(
                                "Duplicate phone number in file: " + p
                        );
                    }
                }

                /* =========================
                   DB DUPLICATES
                   ========================= */
                if (options.isSkipDuplicates()
                        && supplierService.existsByNameIgnoreCase(name)) {
                    throw new IllegalArgumentException(
                            "Supplier already exists: " + name
                    );
                }

                /* =========================
                   CATEGORY RESOLUTION
                   ========================= */
                Set<Long> categoryIds = Set.of();

                if (row.getCategoryNames() != null && !row.getCategoryNames().isEmpty()) {
                    categoryIds = row.getCategoryNames().stream()
                            .map(String::trim)
                            .filter(s -> !s.isBlank())
                            .map(catName ->
                                    categoryRepository.findByNameIgnoreCase(catName)
                                            .orElseThrow(() ->
                                                    new IllegalArgumentException(
                                                            "Unknown category: " + catName
                                                    )
                                            ).getId()
                            )
                            .collect(Collectors.toSet());
                }

                /* =========================
                   BUILD DTO
                   ========================= */
                SupplierCreateDTO dto = SupplierCreateDTO.builder()
                        .name(name)
                        .email(emails)
                        .phoneNumber(phones)
                        .address(row.getAddress())
                        .region(row.getRegion())
                        .categoryIds(categoryIds)
                        .build();

                if (options.isDryRun()) {
                    // 🧪 simulate
                    result.addSuccess(
                            SupplierDTO.builder()
                                    .name(dto.getName())
                                    .email(dto.getEmail())
                                    .phoneNumber(dto.getPhoneNumber())
                                    .address(dto.getAddress())
                                    .region(dto.getRegion())
                                    .build()
                    );
                } else {
                    SupplierDTO saved =
                            supplierService.createSupplier(dto, creatorUsername);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        /* =========================
           HARD ROLLBACK GUARANTEE
           ========================= */
        if (options.isDryRun()) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();
        }

        return result;
    }

    /* =========================
       HELPERS
       ========================= */

    private void validate(SupplierBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getEmail() == null || row.getEmail().isEmpty())
            throw new IllegalArgumentException("At least one email is required");

        if (row.getPhoneNumber() == null || row.getPhoneNumber().isEmpty())
            throw new IllegalArgumentException("At least one phone number is required");
    }

    private Set<String> normalizeSet(Set<String> raw, boolean lowercase) {
        if (raw == null) return Set.of();

        return raw.stream()
                .flatMap(v -> Arrays.stream(v.split(",")))
                .map(String::trim)
                .filter(s -> !s.isBlank())
                .map(s -> lowercase ? s.toLowerCase() : s)
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    private String toTitleCase(String input) {
        String[] parts = input.trim().toLowerCase().split("\\s+");
        return Arrays.stream(parts)
                .map(p -> p.substring(0, 1).toUpperCase() + p.substring(1))
                .collect(Collectors.joining(" "));
    }
}