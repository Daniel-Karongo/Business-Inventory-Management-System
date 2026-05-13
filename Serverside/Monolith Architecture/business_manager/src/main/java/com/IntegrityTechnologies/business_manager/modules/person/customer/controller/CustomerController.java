package com.IntegrityTechnologies.business_manager.modules.person.customer.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.modules.person.customer.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Gender;
import com.IntegrityTechnologies.business_manager.modules.person.customer.service.CustomerBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantSupervisorOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.util.UUID;

@Tag(name = "Customers")
@RestController
@RequestMapping("/api/customers")
@RequiredArgsConstructor
@TenantUserOnly
public class CustomerController {

    private final CustomerService customerService;
    private final CustomerBulkService bulkService;

    /* ====================================
       BULK IMPORT
       ==================================== */

    @TenantManagerOnly
    @PostMapping("/bulk")
    public ResponseEntity<?> bulkImport(
            @RequestParam UUID branchId,
            @RequestBody BulkRequest<CustomerBulkRow> request
    ) {

        return ResponseEntity.ok(
                bulkService.importCustomers(
                        branchId,
                        request
                )
        );
    }

    /* ====================================
       CREATE
       ==================================== */

    @PostMapping
    public ResponseEntity<CustomerResponse> createCustomer(
            @RequestParam UUID branchId,
            @Valid @RequestBody CustomerRequest req
    ) {

        CustomerResponse created =
                customerService.createCustomer(
                        branchId,
                        req
                );

        return ResponseEntity
                .created(URI.create("/api/customers/" + created.getId()))
                .body(created);
    }

    /* ====================================
       LIST
       ==================================== */

    @TenantManagerOnly
    @GetMapping
    public ResponseEntity<Page<CustomerResponse>> listCustomers(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) CustomerType type,
            @RequestParam(required = false) Gender gender,
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                customerService.listCustomers(
                        branchId,
                        page,
                        size,
                        type,
                        gender,
                        deleted
                )
        );
    }

    /* ====================================
       READ
       ==================================== */

    @GetMapping("/{id}")
    public ResponseEntity<CustomerResponse> getCustomer(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                customerService.getCustomer(
                        branchId,
                        id
                )
        );
    }

    @GetMapping("/lookup")
    public ResponseEntity<CustomerResponse> lookupByPhone(
            @RequestParam UUID branchId,
            @RequestParam String phone
    ) {

        return customerService.findByPhone(
                        branchId,
                        phone
                )
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    /* ====================================
       UPDATE
       ==================================== */

    @TenantSupervisorOnly
    @PutMapping("/{id}")
    public ResponseEntity<CustomerResponse> updateCustomer(
            @RequestParam UUID branchId,
            @PathVariable UUID id,
            @Valid @RequestBody CustomerRequest req
    ) {

        return ResponseEntity.ok(
                customerService.updateCustomer(
                        branchId,
                        id,
                        req
                )
        );
    }

    /* ====================================
       DELETE / RESTORE
       ==================================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCustomer(
            @RequestParam UUID branchId,
            @PathVariable UUID id,
            @RequestParam(defaultValue = "true") Boolean soft
    ) {

        if (soft) {

            customerService.softDelete(
                    branchId,
                    id
            );

        } else {

            customerService.deleteCustomer(
                    branchId,
                    id
            );
        }

        return ResponseEntity.noContent().build();
    }

    @TenantManagerOnly
    @PatchMapping("/{id}/restore")
    public ResponseEntity<Void> restore(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {

        customerService.restore(
                branchId,
                id
        );

        return ResponseEntity.ok().build();
    }

    /* ====================================
       SEARCH
       ==================================== */

    @TenantManagerOnly
    @GetMapping("/search")
    public ResponseEntity<Page<CustomerResponse>> search(
            @RequestParam UUID branchId,
            @RequestParam String q,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        return ResponseEntity.ok(
                customerService.search(
                        branchId,
                        q,
                        page,
                        size
                )
        );
    }

    /* ====================================
       RELATED DATA
       ==================================== */

    @GetMapping("/{id}/payments")
    public ResponseEntity<Object> customerPayments(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                customerService.getCustomerPayments(
                        branchId,
                        id
                )
        );
    }

    @GetMapping("/{id}/sales")
    public ResponseEntity<Object> customerSales(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                customerService.getCustomerSales(
                        branchId,
                        id
                )
        );
    }

    /* ====================================
       COMMUNICATION
       ==================================== */

    @TenantManagerOnly
    @PostMapping("/send-to-customers")
    public ResponseEntity<?> sendToCustomers(
            @RequestParam UUID branchId,
            @RequestBody CustomerSmsRequest req
    ) {

        return ResponseEntity.ok(
                customerService.sendToCustomers(
                        branchId,
                        req.getCustomerIds(),
                        req.getMessage()
                )
        );
    }

    /* ====================================
       EXPORT
       ==================================== */

    @TenantManagerOnly
    @GetMapping("/export")
    public ResponseEntity<byte[]> exportCustomers(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String type,
            @RequestParam(required = false) String gender,
            @RequestParam(required = false, defaultValue = "false") Boolean deleted
    ) {

        byte[] csv =
                customerService.exportCsv(
                        branchId,
                        q,
                        type,
                        gender,
                        deleted
                );

        return ResponseEntity.ok()
                .header(
                        HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=customers.csv"
                )
                .contentType(MediaType.TEXT_PLAIN)
                .body(csv);
    }
}