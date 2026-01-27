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

    private Path uploadsRoot;

    /* ============================================================
       INIT
       ============================================================ */

    @PostConstruct
    public void init() {

        Path configured =
                Paths.get(props.getBaseUploadDir())
                        .toAbsolutePath()
                        .normalize();

        uploadsRoot = isWindows()
                ? configured
                : dot(configured.getParent(), "uploads");

        try {
            Files.createDirectories(uploadsRoot);
            hide(uploadsRoot);
        } catch (IOException e) {
            throw new RuntimeException("Failed to init uploads root", e);
        }

        log.info("📁 Uploads root initialized at {}", uploadsRoot);
    }

    /* ============================================================
       ROOT ACCESSORS (🔥 FIXES YOUR ERROR)
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
        Path dir = isWindows()
                ? uploadsRoot.resolve(name)
                : dot(uploadsRoot, name);

        try {
            Files.createDirectories(dir);
            hide(dir);
        } catch (IOException e) {
            throw new RuntimeException("Failed to init module dir: " + dir, e);
        }

        return dir;
    }

    /* ============================================================
       LEGACY / COMPAT API
       ============================================================ */

    public Path initDirectory(Path dir) throws IOException {
        Files.createDirectories(dir);
        hide(dir);
        return dir;
    }

    public Path saveFile(Path dir, String filename, InputStream in) throws IOException {
        Path target = dir.resolve(filename).normalize();
        Files.createDirectories(dir);
        Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
        hide(target);
        return target;
    }

    public void hidePathIfSupported(Path path) {
        hide(path);
    }

    public void hidePath(Path path) throws IOException {
        hide(path);
    }

    public void deleteFile(Path file) throws IOException {
        if (file == null) return;
        Files.deleteIfExists(file);
    }

    public void deleteVisibleOrHiddenDirectory(Path dir) throws IOException {
        deleteDirectory(dir);
    }


    /* ============================================================
       DELETE
       ============================================================ */

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
       HIDING LOGIC (OS-AWARE)
       ============================================================ */

    private void hide(Path path) {
        if (!Files.exists(path)) return;

        try {
            if (isWindows()) {
                Files.setAttribute(path, "dos:hidden", true, LinkOption.NOFOLLOW_LINKS);
            }
            // Linux/macOS handled by dot-folders only
        } catch (Exception e) {
            log.debug("Failed to hide {}", path);
        }
    }

    private Path dot(Path parent, String name) {
        return parent.resolve(name.startsWith(".") ? name : "." + name).normalize();
    }

    private boolean isWindows() {
        return System.getProperty("os.name").toLowerCase().contains("win");
    }
}