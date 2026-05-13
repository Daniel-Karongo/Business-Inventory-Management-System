package com.IntegrityTechnologies.business_manager.security.crypto;

import com.IntegrityTechnologies.business_manager.config.backup.security.BackupKeyProvider;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Base64;

@Slf4j
@Service
@RequiredArgsConstructor
public class CryptoService {

    private static final String AES = "AES";
    private static final String TRANSFORMATION = "AES/GCM/NoPadding";

    private static final int IV_LENGTH = 12;
    private static final int TAG_BITS = 128;

    private final BackupKeyProvider backupKeyProvider;

    private SecretKeySpec secretKey;

    @PostConstruct
    public void init() {

        byte[] decoded =
                Base64.getDecoder()
                        .decode(backupKeyProvider.getKey());

        this.secretKey =
                new SecretKeySpec(decoded, AES);

        log.info(
                "Credential crypto initialized (fingerprint={})",
                java.util.Arrays.hashCode(decoded)
        );
    }

    public String encrypt(String value) {

        if (value == null || value.isBlank()) {
            return value;
        }

        try {

            byte[] iv = new byte[IV_LENGTH];
            new SecureRandom().nextBytes(iv);

            Cipher cipher =
                    Cipher.getInstance(TRANSFORMATION);

            cipher.init(
                    Cipher.ENCRYPT_MODE,
                    secretKey,
                    new GCMParameterSpec(TAG_BITS, iv)
            );

            byte[] encrypted =
                    cipher.doFinal(
                            value.getBytes(StandardCharsets.UTF_8)
                    );

            ByteBuffer buffer =
                    ByteBuffer.allocate(iv.length + encrypted.length);

            buffer.put(iv);
            buffer.put(encrypted);

            return Base64.getEncoder()
                    .encodeToString(buffer.array());

        } catch (Exception e) {

            throw new IllegalStateException(
                    "Credential encryption failed",
                    e
            );
        }
    }

    public String decrypt(String value) {

        if (value == null || value.isBlank()) {
            return value;
        }

        try {

            byte[] decoded =
                    Base64.getDecoder()
                            .decode(value);

            ByteBuffer buffer =
                    ByteBuffer.wrap(decoded);

            byte[] iv = new byte[IV_LENGTH];
            buffer.get(iv);

            byte[] encrypted =
                    new byte[buffer.remaining()];

            buffer.get(encrypted);

            Cipher cipher =
                    Cipher.getInstance(TRANSFORMATION);

            cipher.init(
                    Cipher.DECRYPT_MODE,
                    secretKey,
                    new GCMParameterSpec(TAG_BITS, iv)
            );

            byte[] plain =
                    cipher.doFinal(encrypted);

            return new String(
                    plain,
                    StandardCharsets.UTF_8
            );

        } catch (Exception e) {

            throw new IllegalStateException(
                    "Credential decryption failed",
                    e
            );
        }
    }
}