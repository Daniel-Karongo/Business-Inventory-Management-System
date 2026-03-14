package com.IntegrityTechnologies.business_manager.modules.platform.security.annotation;

import java.lang.annotation.*;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface TenantSuperuserOnly {
}