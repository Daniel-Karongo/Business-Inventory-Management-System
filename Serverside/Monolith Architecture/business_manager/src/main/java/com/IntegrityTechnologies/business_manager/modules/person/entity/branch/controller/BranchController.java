package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchBulkRow;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service.BranchBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service.BranchService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Tag(name = "Branches")
@RestController
@RequestMapping("/api/branches")
@RequiredArgsConstructor
public class BranchController {

    private final BranchService branchService;
    private final BranchBulkService bulkService;

    @PostMapping("/import")
    public ResponseEntity<BulkResult<BranchDTO>> importBranches(
            @RequestBody BulkRequest<BranchBulkRow> request,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                bulkService.importBranches(request, authentication)
        );
    }

    @PostMapping
    public ResponseEntity<ApiResponse> create(
            @RequestBody BranchDTO request,
            Authentication authentication
    ) {
        ApiResponse response = new ApiResponse("success", "Branch created successfully", branchService.create(request, authentication));
        return ResponseEntity.ok(response);
    }

//    @PostMapping("/bulk")
//    public ResponseEntity<ApiResponse> createInBulk(
//            @RequestBody List<BranchDTO> branches,
//            Authentication authentication
//    ) {
//        List<BranchDTO> branchDTOS = new ArrayList<>();
//        for(BranchDTO branch: branches) {
//            branchDTOS.add(branchService.create(branch, authentication));
//        }
//        ApiResponse response = new ApiResponse("success", "Branches created successfully", branchDTOS);
//        return ResponseEntity.ok(response);
//    }

    @GetMapping
    public ResponseEntity<List<BranchDTO>> getAll(@RequestParam(required = false) Boolean deleted) {
        return ResponseEntity.ok(branchService.getAll(deleted));
    }

    @GetMapping("/{id}")
    public ResponseEntity<BranchDTO> getById(@PathVariable UUID id) {
        return ResponseEntity.ok(branchService.getById(id));
    }

    @PatchMapping("/{id}")
    public ResponseEntity<BranchDTO> update(
            @PathVariable UUID id,
            @RequestBody BranchDTO request,
            Authentication authentication
    ) {

        return ResponseEntity.ok(branchService.update(id, request, authentication));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse> deleteBranch(
            @PathVariable UUID id,
            @RequestParam Boolean soft,
            Authentication authentication
    ) {
        branchService.deleteBranch(id, soft, authentication);
        return ResponseEntity.ok(new ApiResponse("success", "Branch " + id.toString() + " deleted successfully"));
    }

    @DeleteMapping("/bulk")
    public ResponseEntity<ApiResponse> deleteBranchesInBulk(
            @RequestBody List<UUID> ids,
            @RequestParam Boolean soft,
            Authentication authentication
    ) {
        for(UUID id: ids) {
            branchService.deleteBranch(id, soft,authentication);
        }
        return ResponseEntity.ok(new ApiResponse("success", "Branches deleted successfully"));
    }

    @PatchMapping("restore/{id}")
    public ResponseEntity<?> restoreBranch(
            @PathVariable UUID id,
            Authentication authentication
    ) {
        branchService.restoreBranch(id, authentication);
        return ResponseEntity.ok(new ApiResponse("success", "Branch " + id.toString() + " restored successfully"));
    }

    @PatchMapping("/restore/bulk")
    public ResponseEntity<?> restoreBranchesInBulk(
            @RequestBody List<UUID> ids,
            Authentication authentication
    ) {
        for(UUID id: ids) {
            branchService.restoreBranch(id, authentication);
        }
        return ResponseEntity.ok(new ApiResponse("success", "Branches restored successfully"));
    }
}