package com.IntegrityTechnologies.business_manager.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BranchContextFilter extends OncePerRequestFilter {

    private final BranchResolver branchResolver;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        try {

            String branchParam = request.getParameter("branchId");

            UUID branchId = null;

            if (branchParam != null && !branchParam.isBlank()) {
                branchId = UUID.fromString(branchParam);
            }

            UUID resolved = branchResolver.resolveBranch(branchId);

            BranchContext.set(resolved);

            filterChain.doFilter(request, response);

        } finally {
            BranchContext.clear();
        }
    }
}