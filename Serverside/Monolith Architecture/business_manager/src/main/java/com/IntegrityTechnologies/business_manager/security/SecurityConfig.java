package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.filter.JwtAuthenticationFilter;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.filter.JwtExceptionHandlerFilter;
import com.IntegrityTechnologies.business_manager.security.acl.config.PermissionSecurityFilter;
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
    private final JwtExceptionHandlerFilter jwtExceptionHandlerFilter;
    private final CustomAuthenticationEntryPoint authenticationEntryPoint;
    private final PermissionSecurityFilter permissionSecurityFilter;


    public SecurityConfig(
            JwtAuthenticationFilter jwtAuthFilter,
            JwtExceptionHandlerFilter jwtExceptionHandlerFilter,
            CustomAuthenticationEntryPoint authenticationEntryPoint,
            PermissionSecurityFilter permissionSecurityFilter
    ) {
        this.jwtAuthFilter = jwtAuthFilter;
        this.jwtExceptionHandlerFilter = jwtExceptionHandlerFilter;
        this.authenticationEntryPoint = authenticationEntryPoint;
        this.permissionSecurityFilter = permissionSecurityFilter;
    }

    /* =====================================================
       CORS (COOKIE-COMPATIBLE)
       ===================================================== */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {

        CorsConfiguration config = new CorsConfiguration();

        config.setAllowedOriginPatterns(List.of(
                "http://localhost:4200",
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
                                "/api/payments/mpesa/c2b/**"
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

                // ---- FILTER ORDER ----
                .addFilterBefore(
                        jwtExceptionHandlerFilter,
                        UsernamePasswordAuthenticationFilter.class
                )
                .addFilterBefore(
                        jwtAuthFilter,
                        UsernamePasswordAuthenticationFilter.class
                ).addFilterAfter(
                        permissionSecurityFilter,
                        JwtAuthenticationFilter.class
                )
        ;

        return http.build();
    }

    /* =====================================================
       PASSWORDS
       ===================================================== */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    /* =====================================================
       AUTH MANAGER
       ===================================================== */
    @Bean
    public AuthenticationManager authenticationManager(
            AuthenticationConfiguration config
    ) throws Exception {
        return config.getAuthenticationManager();
    }
}