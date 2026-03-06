package com.IntegrityTechnologies.business_manager.modules.platform.security.annotation;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface TenantSupervisorOnly {
}