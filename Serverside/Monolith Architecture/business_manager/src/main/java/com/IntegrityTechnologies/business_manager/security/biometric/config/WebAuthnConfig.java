package com.IntegrityTechnologies.business_manager.security.biometric.config;

import com.IntegrityTechnologies.business_manager.security.biometric.repository.WebAuthnCredentialRepository;
import com.yubico.webauthn.*;
import com.yubico.webauthn.data.RelyingPartyIdentity;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Set;

@Configuration
public class WebAuthnConfig {

    @Bean
    public RelyingParty relyingParty(
            WebAuthnCredentialRepository credentialRepository,
            @Value("${webauthn.rp-id}") String rpId,
            @Value("${webauthn.rp-name}") String rpName,
            @Value("${webauthn.origin}") String origin
    ) {

        return RelyingParty.builder()
                .identity(RelyingPartyIdentity.builder()
                        .id(rpId)
                        .name(rpName)
                        .build())
                .credentialRepository(credentialRepository)
                .origins(Set.of(origin))
                .build();
    }
}