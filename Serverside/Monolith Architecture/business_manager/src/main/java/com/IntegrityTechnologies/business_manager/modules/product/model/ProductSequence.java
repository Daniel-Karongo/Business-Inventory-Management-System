package com.IntegrityTechnologies.business_manager.modules.product.model;

import jakarta.persistence.*;
import lombok.Data;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;

@Entity
@Table(name = "product_sequences",
        uniqueConstraints = @UniqueConstraint(columnNames = {"category_id"}))
@Data
public class ProductSequence {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "category_id", nullable = false, unique = true)
    private Long categoryId;

    /**
     * Last allocated sequence number for the category.
     * Start at 0, increment to produce next value.
     */
    @Column(name = "last_seq_value", nullable = false)
    private Long lastValue = 0L;

    // optional version for optimistic locking if desired
    @Version
    private Long version;
}