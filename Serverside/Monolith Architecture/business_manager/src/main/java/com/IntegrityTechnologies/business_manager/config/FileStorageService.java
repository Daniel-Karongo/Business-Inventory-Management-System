package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.util.Comparator;

@Slf4j
@Service
@RequiredArgsConstructor
public class FileStorageService {

    private final FileStorageProperties props;
    private final TransactionalFileManager transactionalFileManager;

    private Path storageRoot;
    private Path uploadsRoot;

    /* ============================================================
       INIT (RELATIVE TO JAR / IDE)
       ============================================================ */

    @PostConstruct
    public void init() {

        Path workingDir = Paths.get(System.getProperty("user.dir"))
                .toAbsolutePath()
                .normalize();

        storageRoot = resolveHidden(workingDir.resolve(props.getRootDir()));
        uploadsRoot = resolveHidden(storageRoot.resolve(props.getUploadsDir()));

        try {
            // 1️⃣ Storage root (hidden)
            Files.createDirectories(storageRoot);
            hidePathIfSupported(storageRoot);

            // 2️⃣ Uploads root (hidden)
            Files.createDirectories(uploadsRoot);
            hidePathIfSupported(uploadsRoot);

        } catch (IOException e) {
            throw new RuntimeException("Failed to init storage or uploads root", e);
        }

        log.info("📁 Storage root initialized at {}", storageRoot);
        log.info("📁 Uploads root initialized at {}", uploadsRoot);
    }

    /* ============================================================
       ROOT ACCESSORS (UNCHANGED API)
       ============================================================ */

    public Path productRoot() {
        return moduleRoot("products");
    }

    public Path userRoot() {
        return moduleRoot("users");
    }

    public Path supplierRoot() {
        return moduleRoot("suppliers");
    }

    private Path moduleRoot(String name) {
        Path dir = resolveHidden(uploadsRoot.resolve(name));
        try {
            Files.createDirectories(dir);
            hidePathIfSupported(dir);
        } catch (IOException e) {
            throw new RuntimeException("Failed to init module dir: " + dir, e);
        }
        return dir;
    }

    /* ============================================================
       INTERNAL STORAGE (BACKUPS ETC)
       ============================================================ */

    public Path internalRoot(String name) {
        Path dir = resolveHidden(storageRoot.resolve(name));
        try {
            Files.createDirectories(dir);
            hidePathIfSupported(dir);
        } catch (IOException e) {
            throw new RuntimeException("Failed to init internal dir: " + dir, e);
        }
        return dir;
    }

    public Path toRelative(Path absolutePath) {

        Path normalized = absolutePath.normalize();

        if (!normalized.startsWith(storageRoot)) {
            throw new IllegalArgumentException("Path is outside storage root");
        }

        return storageRoot.relativize(normalized);
    }
    public Path resolveRelative(String relativePath) {

        Path resolved = storageRoot
                .resolve(relativePath)
                .normalize();

        if (!resolved.startsWith(storageRoot)) {
            throw new IllegalArgumentException("Invalid backup path");
        }

        return resolved;
    }

    /* ============================================================
       LEGACY / COMPAT METHODS (RESTORED)
       ============================================================ */

    public void hidePathIfSupported(Path path) {
        hidePath(path);
    }

    public void hidePath(Path path) {
        if (!Files.exists(path)) return;

        try {
            if (isWindows()) {
                Files.setAttribute(path, "dos:hidden", true, LinkOption.NOFOLLOW_LINKS);
            }
            // Linux/macOS: handled by dot-folders
        } catch (Exception ignored) {}
    }

    public void deleteVisibleOrHiddenDirectory(Path dir) throws IOException {
        deleteDirectory(dir);
    }

    /* ============================================================
       FILE OPS (UNCHANGED)
       ============================================================ */

    public Path initDirectory(Path dir) throws IOException {
        Files.createDirectories(dir);
        hidePathIfSupported(dir);
        return dir;
    }

    public Path saveFile(Path dir, String filename, InputStream in) throws IOException {
        Path target = dir.resolve(filename).normalize();
        Files.createDirectories(dir);
        Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
        hidePathIfSupported(target);
        return target;
    }

    public void deleteFile(Path file) throws IOException {
        if (file == null) return;
        Files.deleteIfExists(file);
    }

    public void deleteDirectoryAfterCommit(Path dir, String module) {
        transactionalFileManager.runAfterCommit(() -> {
            try { deleteDirectory(dir); }
            catch (IOException ignored) {}
        });
    }

    public void deleteDirectory(Path dir) throws IOException {
        if (!Files.exists(dir)) return;

        Files.walk(dir)
                .sorted(Comparator.reverseOrder())
                .forEach(p -> {
                    try { Files.deleteIfExists(p); }
                    catch (IOException ignored) {}
                });
    }

    /* ============================================================
       INTERNAL HELPERS (PRIVATE)
       ============================================================ */

    private Path resolveHidden(Path path) {
        if (isWindows()) return path;

        Path parent = path.getParent();
        String name = path.getFileName().toString();

        if (name.startsWith(".")) return path;
        return parent.resolve("." + name);
    }

    private boolean isWindows() {
        return System.getProperty("os.name").toLowerCase().contains("win");
    }
}