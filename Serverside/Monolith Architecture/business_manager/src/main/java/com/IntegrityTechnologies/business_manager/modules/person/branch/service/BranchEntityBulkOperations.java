package com.IntegrityTechnologies.business_manager.modules.person.branch.service;

import jakarta.persistence.Entity;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@Transactional
public class BranchEntityBulkOperations {

    @PersistenceContext
    private EntityManager entityManager;

    private String entityName(Class<?> entityClass) {

        Entity entity =
                entityClass.getAnnotation(Entity.class);

        if (entity != null &&
                entity.name() != null &&
                !entity.name().isBlank()) {
            return entity.name();
        }

        return entityClass.getSimpleName();
    }

    public int branchDelete(
            Class<?> entityClass,
            UUID tenantId,
            UUID branchId
    ) {

        String jpql = """
            update %s e
               set e.deleted = true,
                   e.branchDeleted = true,
                   e.deletedAt = :now,
                   e.branchDeletedAt = :now
             where e.tenantId = :tenantId
               and e.branchId = :branchId
               and e.deleted = false
            """.formatted(
                entityName(entityClass)
        );

        return entityManager
                .createQuery(jpql)
                .setParameter(
                        "now",
                        LocalDateTime.now()
                )
                .setParameter(
                        "tenantId",
                        tenantId
                )
                .setParameter(
                        "branchId",
                        branchId
                )
                .executeUpdate();
    }

    public int branchRestore(
            Class<?> entityClass,
            UUID tenantId,
            UUID branchId
    ) {

        String jpql = """
            update %s e
               set e.deleted = false,
                   e.deletedAt = null,
                   e.branchDeleted = false,
                   e.branchDeletedAt = null
             where e.tenantId = :tenantId
               and e.branchId = :branchId
               and e.branchDeleted = true
            """.formatted(
                entityName(entityClass)
        );

        return entityManager
                .createQuery(jpql)
                .setParameter(
                        "tenantId",
                        tenantId
                )
                .setParameter(
                        "branchId",
                        branchId
                )
                .executeUpdate();
    }

    public int hardDelete(
            Class<?> entityClass,
            UUID tenantId,
            UUID branchId
    ) {

        String jpql = """
            delete
              from %s e
             where e.tenantId = :tenantId
               and e.branchId = :branchId
            """.formatted(
                entityName(entityClass)
        );

        return entityManager
                .createQuery(jpql)
                .setParameter(
                        "tenantId",
                        tenantId
                )
                .setParameter(
                        "branchId",
                        branchId
                )
                .executeUpdate();
    }
}