package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "receipt_sequences")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ReceiptSequence {

    @Id
    @Column(length = 50)
    private String name;

    @Column(nullable = false)
    private Long nextValue;
}