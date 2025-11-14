package com.IntegrityTechnologies.business_manager.modules.user.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "user_image_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserImageAudit {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID userId;

    private String username;
    private String fileName;
    private String filePath;

    private String action;       // UPLOAD, DELETE
    private String reason;
    private UUID performedById;
    private String performedByUsername;
    private LocalDateTime timestamp;
}