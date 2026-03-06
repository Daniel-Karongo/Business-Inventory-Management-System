package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import java.lang.reflect.Parameter;
import java.util.UUID;

@Aspect
@Component
public class TenantRepositoryAspect {

    @Around("execution(* com.IntegrityTechnologies.business_manager..repository..*(..))")
    public Object injectTenantId(ProceedingJoinPoint joinPoint) throws Throwable {

        Object[] args = joinPoint.getArgs();

        Parameter[] params =
                ((org.aspectj.lang.reflect.MethodSignature) joinPoint.getSignature())
                        .getMethod()
                        .getParameters();

        UUID tenantId = TenantContext.getOrNull();

        for (int i = 0; i < params.length; i++) {

            if (params[i].isAnnotationPresent(TenantScoped.class)
                    && args[i] == null) {

                args[i] = tenantId;

            }
        }

        return joinPoint.proceed(args);
    }
}