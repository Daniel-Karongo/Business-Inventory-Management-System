package com.IntegrityTechnologies.business_manager.security.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class AuthRateLimitFilter extends OncePerRequestFilter {

    private final Map<String, Counter> counters = new ConcurrentHashMap<>();

    private static final int LIMIT = 10; // per minute
    private static final long WINDOW_SECONDS = 60;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String path = request.getRequestURI();

        // Only protect auth endpoints
        if (!path.startsWith("/api/auth")) {
            filterChain.doFilter(request, response);
            return;
        }

        String ip = request.getRemoteAddr();
        String deviceId = request.getParameter("deviceId");

        String key = ip + "|" + (deviceId != null ? deviceId : "unknown");

        Counter counter = counters.computeIfAbsent(key, k -> new Counter());

        synchronized (counter) {

            long now = Instant.now().getEpochSecond();

            if (now - counter.windowStart >= WINDOW_SECONDS) {
                counter.windowStart = now;
                counter.count = 0;
            }

            counter.count++;

            if (counter.count > LIMIT) {

                response.setStatus(429);
                response.setContentType("application/json");

                response.getWriter().write("""
                    {
                      "status":429,
                      "error":"Too Many Requests",
                      "message":"Too many authentication attempts"
                    }
                """);

                return;
            }
        }

        filterChain.doFilter(request, response);
    }

    private static class Counter {
        long windowStart = Instant.now().getEpochSecond();
        int count = 0;
    }
}