package com.IntegrityTechnologies.business_manager.modules.platform.security.annotation;

import org.springframework.security.access.prepost.PreAuthorize;

import java.lang.annotation.*;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@PreAuthorize("@platformSecurity.isTenantUser()")
@Documented
public @interface TenantUserOnly {
}