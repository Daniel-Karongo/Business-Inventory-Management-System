package com.IntegrityTechnologies.business_manager.modules.auth.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.IntegrityTechnologies.business_manager.exception.InvalidTokenException;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.security.SignatureException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Component
public class JwtExceptionHandlerFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
            throws ServletException, IOException {
        try {
            filterChain.doFilter(request, response);
        } catch (InvalidTokenException | SignatureException ex) {
            buildErrorResponse(response, "Invalid JWT signature", HttpStatus.UNAUTHORIZED);
        } catch (ExpiredJwtException ex) {
            buildErrorResponse(response, "JWT token has expired", HttpStatus.UNAUTHORIZED);
        } catch (MalformedJwtException ex) {
            buildErrorResponse(response, "Malformed JWT token", HttpStatus.BAD_REQUEST);
        } catch (Exception ex) {
            buildErrorResponse(response, "Authentication error: " + ex.getMessage(), HttpStatus.UNAUTHORIZED);
        }
    }

    private void buildErrorResponse(HttpServletResponse response, String message, HttpStatus status) throws IOException {
        if (!response.isCommitted()) {
            response.setStatus(status.value());
            response.setContentType("application/json");

            Map<String, Object> body = new HashMap<>();
            body.put("status", status.value());
            body.put("error", status.getReasonPhrase());
            body.put("message", message);
            body.put("timestamp", System.currentTimeMillis());

            new ObjectMapper().writeValue(response.getOutputStream(), body);
        }
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getServletPath();
        return path.startsWith("/swagger-ui") || path.startsWith("/v3/api-docs") || path.equals("/swagger-ui.html");
    }
}