package com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "tenant_settings")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TenantSettings {

    @Id
    private UUID tenantId;

    private String timezone;

    private String currency;

    private String locale;

    private String dateFormat;

    private String invoicePrefix;

    private String companyEmail;

    private String logoUrl;

}