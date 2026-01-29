package com.IntegrityTechnologies.business_manager.security.acl.annotation;

import java.lang.annotation.*;

@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RequirePermission {

    /**
     * Permission code, e.g. INVENTORY_TRANSFER
     */
    String value();

    /**
     * Optional: enforce branch scope
     */
    boolean enforceBranch() default false;

    /**
     * Optional: name of request param containing branchId
     */
    String branchParam() default "branchId";
}