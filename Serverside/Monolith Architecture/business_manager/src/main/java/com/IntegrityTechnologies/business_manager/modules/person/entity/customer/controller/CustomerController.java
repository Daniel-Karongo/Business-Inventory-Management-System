package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.controller;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerResponse;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
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

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @PostMapping
    public ResponseEntity<CustomerResponse> createCustomer(@Valid @RequestBody CustomerRequest req) {
        CustomerResponse created = customerService.createCustomer(req);
        return ResponseEntity.created(URI.create("/api/customers/" + created.getId())).body(created);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @GetMapping
    public ResponseEntity<Page<CustomerResponse>> listCustomers(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String q) {
        Page<CustomerResponse> p = customerService.listCustomers(page, size, q);
        return ResponseEntity.ok(p);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @GetMapping("/{id}")
    public ResponseEntity<CustomerResponse> getCustomer(@PathVariable UUID id) {
        return ResponseEntity.ok(customerService.getCustomer(id));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PutMapping("/{id}")
    public ResponseEntity<CustomerResponse> updateCustomer(@PathVariable UUID id, @Valid @RequestBody CustomerRequest req) {
        return ResponseEntity.ok(customerService.updateCustomer(id, req));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCustomer(@PathVariable UUID id) {
        customerService.deleteCustomer(id);
        return ResponseEntity.noContent().build();
    }

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
}