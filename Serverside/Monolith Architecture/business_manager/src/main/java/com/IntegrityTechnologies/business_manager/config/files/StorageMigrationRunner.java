//package com.IntegrityTechnologies.business_manager.config;
//
//import jakarta.annotation.PostConstruct;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Component;
//
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.nio.file.StandardCopyOption;
//import java.util.List;
//import java.util.stream.Stream;
//
//@Slf4j
//@Component   // 🔴 REMOVE AFTER SUCCESSFUL MIGRATION
//@RequiredArgsConstructor
//public class StorageMigrationRunner {
//
//    private final FileStorageService storageService;
//
//    @PostConstruct
//    public void migrate() {
//
//        log.warn("⚠ Starting deep storage migration...");
//
//        try {
//
//            Path workingDir = Paths.get(System.getProperty("user.dir"))
//                    .toAbsolutePath()
//                    .normalize();
//
//            Path newData = workingDir.resolve(".data");
//            Path newUploads = newData.resolve(".uploads");
//            Path newBackups = newData.resolve(".backups");
//            Path newSecurity = newData.resolve(".security");
//
//            Files.createDirectories(newData);
//            Files.createDirectories(newUploads);
//
//            boolean migratedSomething = false;
//
//            /* =========================================================
//               1️⃣ MIGRATE LEGACY data/ → .data/
//               ========================================================= */
//
//            Path legacyData = workingDir.resolve("data");
//            if (Files.exists(legacyData)) {
//                migratedSomething |= moveContentsSafely(legacyData, newData);
//            }
//
//            /* =========================================================
//               2️⃣ MIGRATE VISIBLE uploads/ → .data/.uploads/
//               ========================================================= */
//
//            Path legacyUploads = workingDir.resolve("uploads");
//            if (Files.exists(legacyUploads)) {
//                migratedSomething |= moveContentsSafely(legacyUploads, newUploads);
//            }
//
//            /* =========================================================
//               3️⃣ MIGRATE VISIBLE backups/security → .data
//               ========================================================= */
//
//            moveFolderIfExists(workingDir.resolve("backups"), newBackups);
//            moveFolderIfExists(workingDir.resolve("security"), newSecurity);
//
//            /* =========================================================
//               4️⃣ MIGRATE MODULE FOLDERS → .uploads
//               ========================================================= */
//
//            List.of("products", "users", "suppliers")
//                    .forEach(name -> {
//                        try {
//
//                            Path visible = newUploads.resolve(name);
//                            Path hidden = newUploads.resolve("." + name);
//
//                            if (Files.exists(visible)) {
//
//                                Files.createDirectories(hidden);
//
//                                try (var stream = Files.list(visible)) {
//                                    for (Path entry : stream.toList()) {
//
//                                        Path target = hidden.resolve(entry.getFileName());
//
//                                        if (!Files.exists(target)) {
//                                            log.warn("⚠ Moving '{}' → '{}'", entry, target);
//                                            Files.move(entry, target,
//                                                    StandardCopyOption.REPLACE_EXISTING);
//                                        }
//                                    }
//                                }
//
//                                // delete visible folder if empty
//                                try (var check = Files.list(visible)) {
//                                    if (check.findAny().isEmpty()) {
//                                        Files.deleteIfExists(visible);
//                                    }
//                                }
//                            }
//
//                        } catch (IOException e) {
//                            throw new RuntimeException(e);
//                        }
//                    });
//
//            /* =========================================================
//               5️⃣ ENSURE HIDDEN INTERNAL FOLDERS
//               ========================================================= */
//
//            if (Files.exists(newData.resolve("backups"))
//                    && !Files.exists(newBackups)) {
//                Files.move(newData.resolve("backups"), newBackups);
//            }
//
//            if (Files.exists(newData.resolve("security"))
//                    && !Files.exists(newSecurity)) {
//                Files.move(newData.resolve("security"), newSecurity);
//            }
//
//            /* =========================================================
//               6️⃣ DEEP SECURE EVERYTHING
//               ========================================================= */
//
//            if (Files.exists(newData)) {
//                try (Stream<Path> walk = Files.walk(newData)) {
//                    walk.forEach(storageService::secure);
//                }
//            }
//
//            if (migratedSomething) {
//                log.warn("✅ Storage migration completed successfully.");
//                log.warn("⚠ REMOVE @Component from StorageMigrationRunner now.");
//            } else {
//                log.info("No legacy storage migration required.");
//            }
//
//        } catch (Exception e) {
//            throw new IllegalStateException("Storage migration failed", e);
//        }
//    }
//
//    /* =========================================================
//       HELPERS
//       ========================================================= */
//
//    private boolean moveContentsSafely(Path source, Path targetRoot) throws IOException {
//
//        if (!Files.exists(source)) return false;
//
//        boolean moved = false;
//
//        try (Stream<Path> paths = Files.list(source)) {
//
//            for (Path entry : paths.toList()) {
//
//                Path target = targetRoot.resolve(entry.getFileName());
//
//                if (!Files.exists(target)) {
//                    log.warn("⚠ Moving '{}' → '{}'", entry, target);
//                    Files.move(entry, target, StandardCopyOption.REPLACE_EXISTING);
//                    moved = true;
//                }
//            }
//        }
//
//        deleteIfEmpty(source);
//
//        return moved;
//    }
//
//    private void moveFolderIfExists(Path source, Path target) throws IOException {
//
//        if (!Files.exists(source)) return;
//
//        Files.createDirectories(target.getParent());
//
//        if (!Files.exists(target)) {
//            log.warn("⚠ Moving '{}' → '{}'", source.getFileName(), target);
//            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
//        } else {
//            moveContentsSafely(source, target);
//        }
//
//        deleteIfEmpty(source);
//    }
//
//    private void deleteIfEmpty(Path dir) throws IOException {
//        if (Files.exists(dir) && Files.isDirectory(dir)) {
//            try (Stream<Path> stream = Files.list(dir)) {
//                if (stream.findAny().isEmpty()) {
//                    Files.deleteIfExists(dir);
//                }
//            }
//        }
//    }
//}