package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.filter.JwtAuthenticationFilter;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.filter.JwtExceptionHandlerFilter;
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

    public SecurityConfig(
            JwtAuthenticationFilter jwtAuthFilter,
            JwtExceptionHandlerFilter jwtExceptionHandlerFilter,
            CustomAuthenticationEntryPoint authenticationEntryPoint
    ) {
        this.jwtAuthFilter = jwtAuthFilter;
        this.jwtExceptionHandlerFilter = jwtExceptionHandlerFilter;
        this.authenticationEntryPoint = authenticationEntryPoint;
    }

    /* =====================================================
       CORS (COOKIE-COMPATIBLE)
       ===================================================== */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {

        CorsConfiguration config = new CorsConfiguration();

        // ⚠️ Must be explicit when allowCredentials = true
        config.setAllowedOrigins(List.of("http://localhost:4200"));

        config.setAllowedMethods(List.of(
                "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"
        ));

        config.setAllowedHeaders(List.of("*"));

        // Cookies are NOT exposed via headers; this is safe
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
                // ---- CORS ----
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))

                // ---- CSRF ----
                // Disabled because:
                // - JWT is HttpOnly
                // - SameSite=Lax
                // - API is stateless
                .csrf(AbstractHttpConfigurer::disable)

                // ---- STATELESS ----
                .sessionManagement(
                        session -> session.sessionCreationPolicy(
                                SessionCreationPolicy.STATELESS
                        )
                )

                // ---- AUTHORIZATION RULES ----
                .authorizeHttpRequests(auth -> auth

                        // 🔓 AUTH
                        .requestMatchers(
                                "/api/auth/login",
                                "/api/auth/login/bulk",
                                "/api/auth/password-reset/**",
                                "/api/auth/forgot-password/**",
                                "/api/auth/reset-password/**"
                        ).permitAll()

                        // 🔓 MPESA CALLBACKS (CRITICAL)
                        .requestMatchers(
                                "/api/payments/mpesa/stk/callback",
                                "/api/payments/mpesa/c2b/**"
                        ).permitAll()

                        // 🔓 PUBLIC (LOGIN UI DEPENDENCIES)
                        .requestMatchers(HttpMethod.GET, "/api/branches").permitAll()

                        // 🔓 SWAGGER
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html"
                        ).permitAll()

                        // 🔐 EVERYTHING ELSE
                        .anyRequest().authenticated()
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
                );

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