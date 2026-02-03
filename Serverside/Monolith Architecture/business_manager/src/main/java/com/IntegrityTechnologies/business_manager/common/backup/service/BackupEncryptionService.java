package com.IntegrityTechnologies.business_manager.common.backup.service;

import com.IntegrityTechnologies.business_manager.common.backup.security.BackupKeyProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.spec.SecretKeySpec;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

@Service
@RequiredArgsConstructor
public class BackupEncryptionService {

    private final BackupKeyProvider keyProvider;

    /* =========================================================
       ENCRYPT
       ========================================================= */

    public Path encrypt(Path input) throws Exception {

        if (input.getFileName().toString().endsWith(".enc")) {
            return input; // prevent double encryption
        }

        byte[] keyBytes = decodeKey();
        SecretKeySpec key = new SecretKeySpec(keyBytes, "AES");

        Cipher cipher = Cipher.getInstance("AES");
        cipher.init(Cipher.ENCRYPT_MODE, key);

        Path encrypted = input.resolveSibling(input.getFileName() + ".enc");

        try (CipherOutputStream cos =
                     new CipherOutputStream(Files.newOutputStream(encrypted), cipher)) {
            Files.copy(input, cos);
        }

        Files.deleteIfExists(input);
        return encrypted;
    }

    /* =========================================================
       DECRYPT
       ========================================================= */

    public Path decryptIfNeeded(Path input) throws Exception {

        if (!input.getFileName().toString().endsWith(".enc")) {
            return input;
        }

        byte[] keyBytes = decodeKey();
        SecretKeySpec key = new SecretKeySpec(keyBytes, "AES");

        Cipher cipher = Cipher.getInstance("AES");
        cipher.init(Cipher.DECRYPT_MODE, key);

        Path decrypted = input.resolveSibling(
                input.getFileName().toString().replaceFirst("\\.enc$", "")
        );

        try (CipherInputStream cis =
                     new CipherInputStream(Files.newInputStream(input), cipher)) {

            Files.copy(cis, decrypted, StandardCopyOption.REPLACE_EXISTING);
        }

        return decrypted;
    }

    /* =========================================================
       KEY HANDLING
       ========================================================= */

    private byte[] decodeKey() {
        // BackupKeyProvider returns Base64
        return java.util.Base64.getDecoder()
                .decode(keyProvider.getKey());
    }
}