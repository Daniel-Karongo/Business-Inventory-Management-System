//package com.IntegrityTechnologies.business_manager.modules.acl.entity;
//
//import com.IntegrityTechnologies.business_manager.modules.person.model.user.Role;
//import jakarta.persistence.*;
//import lombok.*;
//
//import java.util.UUID;
//
//@Entity
//@Table(
//        name = "acl_roles",
//        indexes = {
//                @Index(name = "idx_acl_role_name", columnList = "name", unique = true)
//        }
//)
//@Getter
//@Setter
//@NoArgsConstructor
//@AllArgsConstructor
//@Builder
//public class RoleEntity {
//
//    @Id
//    @GeneratedValue
//    private UUID id;
//
//    /**
//     * Role enum stored as string
//     */
//    @Enumerated(EnumType.STRING)
//    @Column(nullable = false, unique = true)
//    private Role name;
//
//    @Column(nullable = false)
//    private boolean active = true;
//}