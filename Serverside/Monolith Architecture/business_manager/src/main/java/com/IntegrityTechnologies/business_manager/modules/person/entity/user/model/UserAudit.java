package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "user_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserAudit {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID userId;

    private String username;
    private String role;

    private String action;       // CREATE, UPDATE, DELETE, RESTORE
    private String fieldChanged;
    @Column(length = 2000)
    private String oldValue;
    @Column(length = 2000)
    private String newValue;
    private String reason;
    private UUID performedById;
    private String performedByUsername;
    private LocalDateTime timestamp;
}