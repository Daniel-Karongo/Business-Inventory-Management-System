package com.IntegrityTechnologies.business_manager.security.biometric.config;

import com.IntegrityTechnologies.business_manager.security.biometric.repository.WebAuthnCredentialRepository;
import com.yubico.webauthn.RelyingParty;
import com.yubico.webauthn.data.RelyingPartyIdentity;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
@RequiredArgsConstructor
public class RelyingPartyFactory {

    private final WebAuthnCredentialRepository credentialRepository;

    @Value("${webauthn.rp-id}")
    private String rpId;

    @Value("${webauthn.rp-name}")
    private String rpName;

    public RelyingParty forOrigin(String origin) {

        return RelyingParty.builder()
                .identity(RelyingPartyIdentity.builder()
                        .id(rpId)
                        .name(rpName)
                        .build()
                )
                .credentialRepository(credentialRepository) // ✅ ORDER FIX
                .origins(Set.of(origin))
                .build();
    }
}