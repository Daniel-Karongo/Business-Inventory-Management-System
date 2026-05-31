package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductBulkFrontendRequestDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/products/bulk")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductBulkController {

    private final ProductBulkService bulkService;
    private final ProductService productService;

    @TenantManagerOnly
    @PostMapping(value = "/full", consumes = "multipart/form-data")
    public BulkResult<ProductDTO> bulkFullCreate(
            @RequestPart("payload") ProductBulkFrontendRequestDTO dto,
            @RequestPart(value = "files", required = false) List<MultipartFile> files
    ) throws IOException {
        return bulkService.bulkFullCreateFrontend(dto, files);
    }

    @TenantManagerOnly
    @DeleteMapping
    public void bulkSoftDelete(
            @RequestParam UUID branchId,
            @RequestBody BulkActionRequest request
    ) {
        productService.bulkSoftDelete(branchId, request.getIds(), request.getReason());
    }

    @TenantManagerOnly
    @PutMapping("/restore")
    public void bulkRestore(
            @RequestParam UUID branchId,
            @RequestBody BulkActionRequest request
    ) {
        productService.bulkRestore(
                branchId,
                request.getIds(),
                request.getReason(),
                request.getRestoreOptions()
        );
    }

    @PlatformAdminOnly
    @DeleteMapping("/hard")
    public void bulkHardDelete(
            @RequestParam(required = false) UUID branchId,
            @RequestBody BulkActionRequest request
    ) {
        productService.bulkHardDelete(branchId, request.getIds(), request.getReason());
    }
}