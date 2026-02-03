package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SupplierBulkService {

    private final SupplierService supplierService;
    private final CategoryRepository categoryRepository;

    public BulkResult<SupplierDTO> importSuppliers(
            BulkRequest<SupplierBulkRow> request,
            String creatorUsername
    ) {

        BulkResult<SupplierDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            SupplierBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                Set<Long> categoryIds =
                        row.getCategoryNames() == null
                                ? Set.of()
                                : row.getCategoryNames().stream()
                                .map(name ->
                                        categoryRepository.findByNameIgnoreCase(name)
                                                .orElseThrow(() ->
                                                        new IllegalArgumentException(
                                                                "Unknown category: " + name
                                                        )
                                                ).getId()
                                )
                                .collect(Collectors.toSet());

                SupplierCreateDTO dto = SupplierCreateDTO.builder()
                        .name(row.getName())
                        .email(row.getEmail())
                        .phoneNumber(row.getPhoneNumber())
                        .address(row.getAddress())
                        .region(row.getRegion())
                        .categoryIds(categoryIds)
                        .build();

                if (!request.getOptions().isDryRun()) {
                    SupplierDTO saved =
                            supplierService.createSupplier(dto, creatorUsername);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    private void validate(SupplierBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getEmail() == null || row.getEmail().isEmpty())
            throw new IllegalArgumentException("At least one email is required");

        if (row.getPhoneNumber() == null || row.getPhoneNumber().isEmpty())
            throw new IllegalArgumentException("At least one phone number is required");
    }
}