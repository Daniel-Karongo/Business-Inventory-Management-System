package com.IntegrityTechnologies.business_manager.modules.finance.accounts.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(name = "gl_accounts", indexes = {
        @Index(name = "idx_account_code", columnList = "code")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Account {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false, unique = true)
    private String code; // e.g., 1001

    @Column(nullable = false)
    private String name; // e.g., Cash at Bank

    @Enumerated(EnumType.STRING)
    private AccountType type;

    @Column(nullable = false)
    private BigDecimal balance; // running balance

    private boolean active;

    public enum AccountType {
        ASSET, LIABILITY, EQUITY, INCOME, EXPENSE
    }
}