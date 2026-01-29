package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.UUID;

public final class RequestContextBranchResolver {

    private RequestContextBranchResolver() {}

    public static UUID resolveBranchId() {
        HttpServletRequest req = currentRequest();
        if (req == null) return null;

        String branchId = req.getHeader("X-Branch-Id");

        return branchId != null ? UUID.fromString(branchId) : null;
    }

    public static UUID resolveUserId() {
        return SecurityUtils.currentUserId(); // if not present, we add it next
    }

    private static HttpServletRequest currentRequest() {
        ServletRequestAttributes attr =
                (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();

        return attr != null ? attr.getRequest() : null;
    }
}