package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SmsAccount {

    @Id
    @GeneratedValue
    private UUID id;
    private String provider; // AFRICAS_TALKING

    private String username;
    private String apiKey;
    private String senderId;

    private boolean active;
}