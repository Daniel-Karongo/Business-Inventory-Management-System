package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.controller;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerResponse;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerSmsRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Gender;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.util.UUID;

@Tag(name = "Customers")
@RestController
@RequestMapping("/api/customers")
@RequiredArgsConstructor
public class CustomerController {

    private final CustomerService customerService;

    @PostMapping
    public ResponseEntity<CustomerResponse> createCustomer(@Valid @RequestBody CustomerRequest req) {
        CustomerResponse created = customerService.createCustomer(req);
        return ResponseEntity.created(URI.create("/api/customers/" + created.getId())).body(created);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping
    public ResponseEntity<Page<CustomerResponse>> listCustomers(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) CustomerType type,
            @RequestParam(required = false) Gender gender,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(
                customerService.listCustomers(page, size, type, gender, deleted)
        );
    }

    @GetMapping("/{id}")
    public ResponseEntity<CustomerResponse> getCustomer(@PathVariable UUID id) {
        return ResponseEntity.ok(customerService.getCustomer(id));
    }

    @GetMapping("/lookup")
    public ResponseEntity<CustomerResponse> lookupByPhone(@RequestParam String phone) {
        return customerService.findByPhone(phone)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PutMapping("/{id}")
    public ResponseEntity<CustomerResponse> updateCustomer(@PathVariable UUID id, @Valid @RequestBody CustomerRequest req) {
        return ResponseEntity.ok(customerService.updateCustomer(id, req));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCustomer(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "true") Boolean soft
    ) {
        if (soft) customerService.softDelete(id);
        else customerService.deleteCustomer(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/{id}/restore")
    public ResponseEntity<Void> restore(@PathVariable UUID id) {
        customerService.restore(id);
        return ResponseEntity.ok().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping("/search")
    public ResponseEntity<Page<CustomerResponse>> search(
            @RequestParam String q,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {
        return ResponseEntity.ok(customerService.search(q, page, size));
    }

    @GetMapping("/{id}/payments")
    public ResponseEntity<Object> customerPayments(@PathVariable UUID id) {
        return ResponseEntity.ok(customerService.getCustomerPayments(id));
    }

    @GetMapping("/{id}/sales")
    public ResponseEntity<Object> customerSales(@PathVariable UUID id) {
        return ResponseEntity.ok(customerService.getCustomerSales(id));
    }

    @PostMapping("/send-to-customers")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<?> sendToCustomers(
            @RequestBody CustomerSmsRequest req
    ) {
        return ResponseEntity.ok(
                customerService.sendToCustomers(req.getCustomerIds(), req.getMessage())
        );
    }

    @GetMapping("/export")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<byte[]> exportCustomers(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String type,
            @RequestParam(required = false) String gender,
            @RequestParam(required = false, defaultValue = "false") Boolean deleted
    ) {
        byte[] csv = customerService.exportCsv(q, type, gender, deleted);

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=customers.csv")
                .contentType(MediaType.TEXT_PLAIN)
                .body(csv);
    }
}