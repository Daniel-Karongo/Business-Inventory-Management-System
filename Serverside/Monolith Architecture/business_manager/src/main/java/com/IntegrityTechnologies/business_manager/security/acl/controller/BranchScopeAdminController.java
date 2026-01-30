package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditService;
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
    private final AclAuditService audit;

    @GetMapping
    public List<UserBranchScope> list() {
        return repo.findAll();
    }

    @PostMapping
    public UserBranchScope assign(@RequestBody UserBranchScope scope) {

        UserBranchScope saved = repo.save(scope);

        audit.audit(
                "BRANCH_SCOPE",
                "CREATE",
                null,
                saved,
                saved.getId().toString()
        );

        return saved;
    }

    @DeleteMapping("/{id}")
    public void hardDelete(@PathVariable UUID id) {

        UserBranchScope before = repo.findById(id).orElseThrow();
        repo.deleteById(id);

        audit.audit(
                "BRANCH_SCOPE",
                "DELETE",
                before,
                null,
                id.toString()
        );
    }
}