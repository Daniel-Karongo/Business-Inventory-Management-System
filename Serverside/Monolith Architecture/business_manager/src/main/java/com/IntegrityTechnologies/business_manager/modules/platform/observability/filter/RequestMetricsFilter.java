package com.IntegrityTechnologies.business_manager.modules.platform.observability.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.service.TenantMetricsService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class RequestMetricsFilter extends OncePerRequestFilter {

    private final TenantMetricsService metricsService;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        try {

            metricsService.recordRequest();

            filterChain.doFilter(request, response);

        } catch (Exception ex) {

            metricsService.recordError();

            throw ex;
        }
    }
}