package com.IntegrityTechnologies.business_manager.common.backup.util;

import java.io.IOException;
import java.nio.file.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

public final class BackupCompressionUtil {

    private BackupCompressionUtil() {}

    /* =========================================================
       ZIP
       ========================================================= */

    public static Path zip(Path source) throws IOException {

        Path zipPath = Paths.get(source.toString() + ".zip");

        try (ZipOutputStream zos =
                     new ZipOutputStream(Files.newOutputStream(zipPath))) {

            zos.putNextEntry(new ZipEntry(source.getFileName().toString()));
            Files.copy(source, zos);
            zos.closeEntry();
        }

        Files.deleteIfExists(source);
        return zipPath;
    }

    /* =========================================================
       UNZIP (🔥 MISSING METHOD)
       ========================================================= */

    public static Path unzip(Path zipFile) throws IOException {

        Path targetDir = zipFile.getParent();

        try (ZipInputStream zis =
                     new ZipInputStream(Files.newInputStream(zipFile))) {

            ZipEntry entry = zis.getNextEntry();
            if (entry == null) {
                throw new IOException("ZIP file is empty");
            }

            Path extracted = targetDir.resolve(entry.getName());

            Files.copy(zis, extracted, StandardCopyOption.REPLACE_EXISTING);
            zis.closeEntry();

            return extracted;
        }
    }
}