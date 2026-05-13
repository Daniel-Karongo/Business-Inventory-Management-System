package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "mpesa_transactions",
        indexes = {

                @Index(
                        name = "idx_mpesa_tenant_branch",
                        columnList = "tenant_id, branch_id"
                ),

                @Index(
                        name = "idx_mpesa_checkout",
                        columnList = "checkout_request_id"
                ),

                @Index(
                        name = "idx_mpesa_receipt",
                        columnList = "mpesa_receipt_number"
                ),

                @Index(
                        name = "idx_mpesa_sale",
                        columnList = "sale_id"
                )
        },
        uniqueConstraints = {

                @UniqueConstraint(
                        name = "uk_mpesa_checkout",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "checkout_request_id"
                        }
                ),

                @UniqueConstraint(
                        name = "uk_mpesa_receipt",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "mpesa_receipt_number"
                        }
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class MpesaTransaction extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "checkout_request_id")
    private String checkoutRequestId;

    @Column(name = "merchant_request_id")
    private String merchantRequestId;

    @Column(name = "mpesa_receipt_number")
    private String mpesaReceiptNumber;

    @Column(columnDefinition = "BINARY(16)")
    private UUID saleId;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    private String phoneNumber;

    private String status;

    private LocalDateTime timestamp;

    @Column(length = 5000)
    private String rawResponse;
}