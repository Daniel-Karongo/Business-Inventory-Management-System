package com.IntegrityTechnologies.business_manager.modules.person.branch.document.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentUploadDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.mapper.BranchDocumentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocument;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentAudit;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentType;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.repository.BranchDocumentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.repository.BranchDocumentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.web.multipart.MultipartFile;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class BranchDocumentService {

    private final BranchRepository branchRepository;
    private final BranchDocumentRepository documentRepository;
    private final BranchDocumentAuditRepository auditRepository;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final PrivilegesChecker privilegesChecker;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public void uploadDocuments(
            UUID branchId,
            List<BranchDocumentUploadDTO> files,
            Authentication auth
    ) throws IOException {

        Branch branch = getBranch(branchId);

        Path dir =
                fileStorageService.branchDocumentsRoot(
                        branchId
                );

        for (BranchDocumentUploadDTO dto : files) {

            MultipartFile file = dto.getFile();

            if (file == null || file.isEmpty()) {
                continue;
            }

            String filename =
                    UUID.randomUUID()
                            + "_"
                            + file.getOriginalFilename();

            Path saved;

            try (InputStream in =
                         file.getInputStream()) {

                saved =
                        fileStorageService.saveFile(
                                dir,
                                filename,
                                in
                        );

                fileStorageService.secure(saved);
            }

            transactionalFileManager.track(saved);

            String apiUrl =
                    "/api/branches/"
                            + branchId
                            + "/documents/"
                            + filename;

            BranchDocument doc =
                    BranchDocument.builder()
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .fileName(filename)
                            .filePath(apiUrl)
                            .documentType(dto.getDocumentType())
                            .description(dto.getDescription())
                            .uploadedAt(LocalDateTime.now())
                            .logo(dto.getDocumentType() == BranchDocumentType.LOGO)
                            .deleted(false)
                            .build();

            documentRepository.save(doc);

            audit(
                    branch,
                    "UPLOAD",
                    doc.getFileName(),
                    doc.getFilePath(),
                    null,
                    auth
            );
        }

        maintainLogoInvariant(branchId);
    }

    @Transactional
    public List<BranchDocumentDTO> getDocuments(
            UUID branchId,
            Boolean deleted
    ) {

        List<BranchDocument> docs;

        if (deleted == null) {
            docs =
                    documentRepository
                            .findByTenantIdAndBranchIdOrderByUploadedAtDesc(
                                    tenantId(),
                                    branchId
                            );
        } else {
            docs =
                    documentRepository.findByTenantIdAndBranchIdAndDeletedOrderByUploadedAtDesc(
                                    tenantId(),
                                    branchId,
                                    deleted
                            );
        }

        return docs.stream()
                .map(
                        BranchDocumentMapper::toDto
                )
                .toList();
    }

    @Transactional
    public ResponseEntity<Resource> download(
            UUID branchId,
            String filename,
            Authentication auth
    ) throws IOException {

        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        Path dir =
                fileStorageService
                        .branchDocumentsRoot(branchId);

        Path file =
                dir.resolve(filename)
                        .normalize();

        if (!Files.exists(file)) {
            throw new FileNotFoundException(
                    filename
            );
        }

        String contentType =
                Files.probeContentType(file);

        if (contentType == null) {
            contentType =
                    "application/octet-stream";
        }

        audit(
                branch,
                "DOWNLOAD",
                doc.getFileName(),
                doc.getFilePath(),
                null,
                auth
        );

        return ResponseEntity.ok()
                .contentType(
                        MediaType.parseMediaType(
                                contentType
                        )
                )
                .body(
                        new FileSystemResource(
                                file
                        )
                );
    }

    @Transactional
    public void setLogo(
            UUID branchId,
            String filename,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        if (doc.isDeleted()) {
            throw new IllegalStateException(
                    "Deleted document cannot be logo"
            );
        }

        documentRepository.clearExistingLogo(
                tenantId(),
                branchId
        );

        doc.setLogo(true);

        documentRepository.save(doc);

        audit(
                branch,
                "SET_LOGO",
                doc.getFileName(),
                doc.getFilePath(),
                null,
                auth
        );
    }

    @Transactional
    public void updateDescription(
            UUID branchId,
            String filename,
            String description,
            Authentication auth
    ) {
        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        doc.setDescription(description);

        documentRepository.save(doc);

        audit(
                branch,
                "UPDATE_DESCRIPTION",
                doc.getFileName(),
                doc.getFilePath(),
                description,
                auth
        );
    }

    @Transactional
    public void softDelete(
            UUID branchId,
            String filename,
            String reason,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        doc.softDelete();
        doc.setLogo(false);

        documentRepository.save(doc);

        audit(
                branch,
                "SOFT_DELETE",
                filename,
                doc.getFilePath(),
                reason,
                auth
        );

        maintainLogoInvariant(branchId);
    }

    @Transactional
    public void softDeleteAll(
            UUID branchId,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        List<BranchDocument> docs =
                documentRepository
                        .findByTenantIdAndBranchIdOrderByUploadedAtDesc(
                                tenantId(),
                                branchId
                        );

        LocalDateTime now =
                LocalDateTime.now();

        for (BranchDocument doc : docs) {

            doc.setDeleted(true);
            doc.setDeletedAt(now);
            doc.setLogo(false);

            documentRepository.save(doc);

            audit(
                    branch,
                    "SOFT_DELETE_ALL",
                    doc.getFileName(),
                    doc.getFilePath(),
                    null,
                    auth
            );
        }
    }

    @Transactional
    public void restore(
            UUID branchId,
            String filename,
            String reason,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        doc.restore();

        documentRepository.save(doc);

        audit(
                branch,
                "RESTORE",
                filename,
                doc.getFilePath(),
                reason,
                auth
        );

        maintainLogoInvariant(branchId);
    }

    @Transactional
    public void restoreAll(
            UUID branchId,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        List<BranchDocument> docs =
                documentRepository
                        .findByTenantIdAndBranchIdAndDeletedOrderByUploadedAtDesc(
                                tenantId(),
                                branchId,
                                true
                        );

        for (BranchDocument doc : docs) {

            doc.setDeleted(false);
            doc.setDeletedAt(null);

            documentRepository.save(doc);

            audit(
                    branch,
                    "RESTORE_ALL",
                    doc.getFileName(),
                    doc.getFilePath(),
                    null,
                    auth
            );
        }

        maintainLogoInvariant(branchId);
    }

    @Transactional
    public void hardDelete(
            UUID branchId,
            String filename,
            String reason,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        BranchDocument doc =
                documentRepository
                        .findByTenantIdAndBranchIdAndFileName(
                                tenantId(),
                                branchId,
                                filename
                        )
                        .orElseThrow();

        Path dir =
                fileStorageService
                        .branchDocumentsRoot(
                                branchId
                        );

        Path file =
                dir.resolve(filename)
                        .normalize();

        transactionalFileManager.runAfterCommit(
                () -> {
                    try {
                        Files.deleteIfExists(
                                file
                        );
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
        );

        documentRepository.delete(doc);

        audit(
                branch,
                "DELETE",
                doc.getFileName(),
                doc.getFilePath(),
                reason,
                auth
        );

        maintainLogoInvariant(branchId);
    }

    @Transactional
    public void hardDeleteAll(
            UUID branchId,
            Authentication auth
    ) {

        Branch branch =
                getBranch(branchId);

        List<BranchDocument> docs =
                documentRepository
                        .findByTenantIdAndBranchIdOrderByUploadedAtDesc(
                                tenantId(),
                                branchId
                        );

        Path dir =
                fileStorageService
                        .branchDocumentsRoot(
                                branchId
                        );

        transactionalFileManager.runAfterCommit(
                () -> {
                    try {
                        fileStorageService
                                .deleteDirectory(dir);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
        );

        for (BranchDocument doc : docs) {

            audit(
                    branch,
                    "DELETE_ALL",
                    doc.getFileName(),
                    doc.getFilePath(),
                    null,
                    auth
            );
        }

        documentRepository.deleteAll(docs);
    }

    @Transactional
    public List<BranchDocumentAuditDTO> getBranchDocumentAudits(
            UUID branchId
    ) {

        return auditRepository
                .findByTenantIdAndBranchIdOrderByTimestampDesc(
                        tenantId(),
                        branchId
                )
                .stream()
                .map(
                        BranchDocumentAuditDTO::from
                )
                .toList();
    }

    private Branch getBranch(UUID branchId) {
        return branchRepository
                .findByTenantIdAndId(tenantId(), branchId)
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "Branch not found: " + branchId
                        )
                );
    }

    private User actor(Authentication auth) {
        return privilegesChecker.getAuthenticatedUser(auth);
    }

    private void audit(
            Branch branch,
            String action,
            String fileName,
            String filePath,
            String reason,
            Authentication auth
    ) {
        User user = actor(auth);

        auditRepository.save(
                BranchDocumentAudit.builder()
                        .tenantId(tenantId())
                        .branchId(branch.getId())
                        .branchName(branch.getName())
                        .fileName(fileName)
                        .filePath(filePath)
                        .action(action)
                        .reason(reason)
                        .performedById(user.getId())
                        .performedByUsername(user.getUsername())
                        .timestamp(LocalDateTime.now())
                        .build()
        );
    }

    private void maintainLogoInvariant(UUID branchId) {

        List<BranchDocument> active =
                documentRepository.findByTenantIdAndBranchIdAndDeleted(
                                tenantId(),
                                branchId,
                                false
                        );

        if (active.isEmpty()) {
            return;
        }

        List<BranchDocument> logos =
                active.stream()
                        .filter(d -> Boolean.TRUE.equals(d.getLogo()))
                        .toList();

        if (logos.size() > 1) {
            BranchDocument keep = logos.get(0);
            logos.forEach(d -> d.setLogo(false));
            keep.setLogo(true);
            documentRepository.saveAll(logos);
            return;
        }

        if (logos.isEmpty()) {

            Optional<BranchDocument> preferred =
                    active.stream()
                            .filter(d ->
                                    d.getDocumentType()
                                            == BranchDocumentType.LOGO
                            )
                            .findFirst();

            BranchDocument logo =
                    preferred.orElse(active.get(0));

            logo.setLogo(true);

            documentRepository.save(logo);
        }
    }
}