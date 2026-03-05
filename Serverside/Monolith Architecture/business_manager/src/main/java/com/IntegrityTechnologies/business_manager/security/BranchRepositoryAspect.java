package com.IntegrityTechnologies.business_manager.security;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import java.lang.reflect.Parameter;
import java.util.UUID;

@Aspect
@Component
public class BranchRepositoryAspect {

    @Around("execution(* com.IntegrityTechnologies.business_manager..repository..*(..))")
    public Object injectBranchId(ProceedingJoinPoint joinPoint) throws Throwable {

        Object[] args = joinPoint.getArgs();

        Parameter[] params =
                ((org.aspectj.lang.reflect.MethodSignature) joinPoint.getSignature())
                        .getMethod()
                        .getParameters();

        UUID branchId = BranchContext.getOrNull();

        for (int i = 0; i < params.length; i++) {

            if (params[i].isAnnotationPresent(BranchScoped.class)
                    && args[i] == null) {

                args[i] = branchId;
            }
        }

        return joinPoint.proceed(args);
    }
}