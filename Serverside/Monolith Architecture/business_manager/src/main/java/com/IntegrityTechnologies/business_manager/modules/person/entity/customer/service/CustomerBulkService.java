package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CustomerBulkService {

    private final CustomerService customerService;

    public BulkResult<CustomerResponse> importCustomers(
            BulkRequest<CustomerBulkRow> request
    ) {

        BulkResult<CustomerResponse> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            CustomerBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                CustomerRequest req = new CustomerRequest();
                req.setName(row.getName());

                // Phones
                if (row.getPhoneNumbers() != null && !row.getPhoneNumbers().isEmpty()) {
                    req.setPhoneNumbers(row.getPhoneNumbers());
                } else {
                    req.setPhone(row.getPhone());
                }

                // Emails
                if (row.getEmailAddresses() != null && !row.getEmailAddresses().isEmpty()) {
                    req.setEmailAddresses(row.getEmailAddresses());
                } else {
                    req.setEmail(row.getEmail());
                }

                req.setType(row.getType());
                req.setGender(row.getGender());
                req.setAddress(row.getAddress());
                req.setNotes(row.getNotes());

                if (!request.getOptions().isDryRun()) {
                    CustomerResponse saved =
                            customerService.createCustomer(req);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    private void validate(CustomerBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getType() == null)
            throw new IllegalArgumentException("type is required");

        if (row.getType() == CustomerType.INDIVIDUAL && row.getGender() == null)
            throw new IllegalArgumentException("gender is required for INDIVIDUAL");
    }
}