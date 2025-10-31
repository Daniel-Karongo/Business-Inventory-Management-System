package com.IntegrityTechnologies.business_manager.modules.category.model;

import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import java.util.List;

@Entity
@Table(name = "categories")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE categories SET deleted = true WHERE id = ?")
@Where(clause = "deleted = false")
public class Category {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    @OneToMany(mappedBy = "category", cascade = CascadeType.ALL)
    private List<Product> products;

    @Column(nullable = false)
    private boolean deleted = false;
}