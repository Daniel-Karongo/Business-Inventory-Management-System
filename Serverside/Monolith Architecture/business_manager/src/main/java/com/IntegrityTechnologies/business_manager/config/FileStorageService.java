package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.util.Comparator;

@Slf4j
@Service
@RequiredArgsConstructor
public class FileStorageService {

    private final FileStorageProperties props;
    private final TransactionalFileManager transactionalFileManager;

    private Path storageRoot;
    private Path uploadsRoot;

    @PostConstruct
    public void init() {

        Path workingDir = Paths.get(System.getProperty("user.dir"))
                .toAbsolutePath()
                .normalize();

        storageRoot = workingDir.resolve(props.getRootDir()).normalize();
        uploadsRoot = storageRoot.resolve(props.getUploadsDir()).normalize();

        createAndSecure(storageRoot);
        createAndSecure(uploadsRoot);

        log.info("Storage initialized at {}", storageRoot);
    }

    /* ============================================================
       MODULE ROOTS
       ============================================================ */

    public Path productRoot() {
        return moduleRoot(".products");
    }

    public Path userRoot() {
        return moduleRoot(".users");
    }

    public Path supplierRoot() {
        return moduleRoot(".suppliers");
    }

    private Path moduleRoot(String name) {
        Path dir = uploadsRoot.resolve(name).normalize();
        createAndSecure(dir);
        return dir;
    }

    /* ============================================================
       INTERNAL ROOT
       ============================================================ */

    public Path internalRoot(String name) {
        Path dir = storageRoot.resolve("." + name).normalize();
        createAndSecure(dir);
        return dir;
    }

    /* ============================================================
       FILE OPERATIONS
       ============================================================ */

    public Path initDirectory(Path dir) {
        createAndSecure(dir);
        return dir;
    }

    public Path saveFile(Path dir, String filename, InputStream in)
            throws IOException {

        createAndSecure(dir);

        Path target = dir.resolve(filename).normalize();
        validateInsideStorage(target);

        Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
        secure(target);

        return target;
    }

    public void deleteFile(Path file) throws IOException {
        if (file == null) return;
        validateInsideStorage(file);
        Files.deleteIfExists(file);
    }

    public void deleteDirectory(Path dir) throws IOException {

        validateInsideStorage(dir);

        if (!Files.exists(dir)) return;

        Files.walk(dir)
                .sorted(Comparator.reverseOrder())
                .forEach(p -> {
                    try { Files.deleteIfExists(p); }
                    catch (IOException ignored) {}
                });
    }

    /* ============================================================
       SECURITY CORE
       ============================================================ */

    private void createAndSecure(Path dir) {
        try {
            Files.createDirectories(dir);

            // 🔐 Secure ALL path segments inside storage
            Path current = storageRoot;

            for (Path segment : storageRoot.relativize(dir)) {
                current = current.resolve(segment);
                secure(current);
            }

        } catch (IOException e) {
            throw new RuntimeException("Failed to initialize directory: " + dir, e);
        }
    }

    public void secure(Path path) {

        if (!Files.exists(path)) return;

        try {
            if (isWindows()) {
                DosFileAttributeView view =
                        Files.getFileAttributeView(path, DosFileAttributeView.class);

                if (view != null) {
                    view.setHidden(true);
                }
            }
        } catch (Exception e) {
            log.warn("Could not secure path: {}", path);
        }
    }

    private void validateInsideStorage(Path path) {

        if (!path.normalize().startsWith(storageRoot)) {
            throw new SecurityException("Path outside storage root");
        }
    }

    public Path toRelative(Path absolutePath) {
        validateInsideStorage(absolutePath);
        return storageRoot.relativize(absolutePath.normalize());
    }

    public Path resolveRelative(String relativePath) {
        Path resolved = storageRoot.resolve(relativePath).normalize();
        validateInsideStorage(resolved);
        return resolved;
    }

    private boolean isWindows() {
        return System.getProperty("os.name").toLowerCase().contains("win");
    }
}