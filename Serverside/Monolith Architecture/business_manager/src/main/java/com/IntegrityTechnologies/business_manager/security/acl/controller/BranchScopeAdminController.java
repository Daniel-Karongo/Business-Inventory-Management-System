package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.UserBranchScope;
import com.IntegrityTechnologies.business_manager.security.acl.repository.UserBranchScopeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl/branch-scopes")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class BranchScopeAdminController {

    private final UserBranchScopeRepository repo;

    @GetMapping
    public List<UserBranchScope> list() {
        return repo.findAll();
    }

    @PostMapping
    public UserBranchScope assign(@RequestBody UserBranchScope scope) {
        return repo.save(scope);
    }

    @DeleteMapping("/{id}")
    public void revoke(@PathVariable UUID id) {
        repo.deleteById(id);
    }
}