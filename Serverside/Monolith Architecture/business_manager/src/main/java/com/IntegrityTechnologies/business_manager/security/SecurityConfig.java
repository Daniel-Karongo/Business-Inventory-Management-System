package com.IntegrityTechnologies.business_manager.security;

//import com.IntegrityTechnologies.business_manager.modules.acl.config.PermissionSecurityFilter;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.filter.RequestMetricsFilter;
import com.IntegrityTechnologies.business_manager.security.filter.RoleGuardFilter;
import com.IntegrityTechnologies.business_manager.modules.platform.security.rate.TenantRateLimitFilter;
import com.IntegrityTechnologies.business_manager.security.filter.TenantContextFilter;
import com.IntegrityTechnologies.business_manager.security.filter.TenantResolutionFilter;
import com.IntegrityTechnologies.business_manager.security.filter.JwtAuthenticationFilter;
import com.IntegrityTechnologies.business_manager.security.filter.BranchContextFilter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.List;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
public class SecurityConfig {

    private final JwtAuthenticationFilter jwtAuthFilter;
    private final CustomAuthenticationEntryPoint authenticationEntryPoint;
//    private final PermissionSecurityFilter permissionSecurityFilter;
    private final BranchContextFilter branchContextFilter;
    private final TenantContextFilter tenantContextFilter;
    private final RoleGuardFilter roleGuardFilter;
    private final TenantResolutionFilter tenantResolutionFilter;
    private final RequestMetricsFilter requestMetricsFilter;
    private final TenantRateLimitFilter tenantRateLimitFilter;

    public SecurityConfig(
            JwtAuthenticationFilter jwtAuthFilter,
            CustomAuthenticationEntryPoint authenticationEntryPoint,
//            PermissionSecurityFilter permissionSecurityFilter,
            BranchContextFilter branchContextFilter,
            TenantContextFilter tenantContextFilter,
            RoleGuardFilter roleGuardFilter, TenantResolutionFilter tenantResolutionFilter, RequestMetricsFilter requestMetricsFilter, TenantRateLimitFilter tenantRateLimitFilter) {
        this.jwtAuthFilter = jwtAuthFilter;
        this.authenticationEntryPoint = authenticationEntryPoint;
//        this.permissionSecurityFilter = permissionSecurityFilter;
        this.branchContextFilter = branchContextFilter;
        this.tenantContextFilter = tenantContextFilter;
        this.roleGuardFilter = roleGuardFilter;
        this.tenantResolutionFilter = tenantResolutionFilter;
        this.requestMetricsFilter = requestMetricsFilter;
        this.tenantRateLimitFilter = tenantRateLimitFilter;
    }

    /* =====================================================
       CORS (COOKIE-COMPATIBLE)
       ===================================================== */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {

        CorsConfiguration config = new CorsConfiguration();

        config.setAllowedOriginPatterns(List.of(
                "http://localhost:*",
                "http://*.localhost:*",
                "https://*.ngrok-free.app"
        ));

        config.setAllowedMethods(List.of(
                "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"
        ));

        config.setAllowedHeaders(List.of("*"));

        config.setAllowCredentials(true);

        config.setMaxAge(3600L);

        UrlBasedCorsConfigurationSource source =
                new UrlBasedCorsConfigurationSource();

        source.registerCorsConfiguration("/**", config);

        return source;
    }

    /* =====================================================
       SECURITY FILTER CHAIN
       ===================================================== */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {

        http
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                .csrf(AbstractHttpConfigurer::disable)
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .formLogin(AbstractHttpConfigurer::disable)
                .httpBasic(AbstractHttpConfigurer::disable)

                // ---- AUTHORIZATION RULES ----
                .authorizeHttpRequests(auth -> auth

                        // ===== STATIC RESOURCES (CRITICAL FOR NGROK) =====
                        .requestMatchers(
                                "/",
                                "/index.html",
                                "/favicon.ico",
                                "/*.js",
                                "/*.css",
                                "/*.ico",
                                "/*.svg",
                                "/*.png",
                                "/*.jpg",
                                "/*.jpeg",
                                "/*.webp",
                                "/assets/**",
                                "/icons/**",
                                "/media/**",
                                "/manifest.webmanifest"
                        ).permitAll()

                        // ===== SWAGGER =====
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html"
                        ).permitAll()

                        // ===== PUBLIC API =====
                        .requestMatchers(
                                "/api/auth/login",
                                "/api/auth/login/bulk",
                                "/api/auth/password-reset/**",
                                "/api/auth/forgot-password/**",
                                "/api/auth/reset-password/**",
                                "/api/users/register",
                                "/api/payments/mpesa/stk/callback",
                                "/api/payments/mpesa/c2b/**",
                                "/api/platform/settings/logo"
                        ).permitAll()

                        .requestMatchers(HttpMethod.GET, "/api/branches").permitAll()

                        // ===== SECURE API =====
                        .requestMatchers("/api/**").authenticated()

                        .anyRequest().permitAll()
                )

                // ---- ERROR HANDLING ----
                .exceptionHandling(ex ->
                        ex.authenticationEntryPoint(authenticationEntryPoint)
                )

                // =====================================================
                // FILTER ORDER (TOP → BOTTOM)
                // =====================================================
                //
                // 1 TenantResolutionFilter
                // 2 RequestMetricsFilter
                // 3 TenantRateLimitFilter
                // 4 JwtAuthenticationFilter
                // 5 TenantContextFilter
                // 6 BranchContextFilter
                // 7 RoleGuardFilter

                .addFilterBefore(tenantResolutionFilter, UsernamePasswordAuthenticationFilter.class)

                .addFilterAfter(requestMetricsFilter, TenantResolutionFilter.class)

                .addFilterAfter(tenantRateLimitFilter, RequestMetricsFilter.class)

                .addFilterAfter(jwtAuthFilter, TenantRateLimitFilter.class)

                .addFilterAfter(tenantContextFilter, JwtAuthenticationFilter.class)

                .addFilterAfter(branchContextFilter, TenantContextFilter.class)

                .addFilterAfter(roleGuardFilter, BranchContextFilter.class)
        ;

        return http.build();
    }

    @Bean
    public AuthenticationManager authenticationManager(
            AuthenticationConfiguration config
    ) throws Exception {
        return config.getAuthenticationManager();
    }

    /* =====================================================
       PASSWORDS
       ===================================================== */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}