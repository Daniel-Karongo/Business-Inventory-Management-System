package com.IntegrityTechnologies.business_manager.security;

import java.lang.annotation.*;

@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
public @interface TenantScoped {
}