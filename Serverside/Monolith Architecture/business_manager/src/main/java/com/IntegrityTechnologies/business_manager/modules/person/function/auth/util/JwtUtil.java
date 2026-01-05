package com.IntegrityTechnologies.business_manager.modules.person.function.auth.util;

import io.jsonwebtoken.*;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.security.Key;
import java.time.*;
import java.util.Date;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

@Component
public class JwtUtil {

    @Value("${jwt.secret}")
    private String secretKey;

    /**
     * Kept for compatibility but NOT used for expiry anymore.
     * Expiry is always set to next midnight (server timezone).
     */
    @Value("${jwt.expiration}")
    private long jwtExpirationMillis;

    /* =====================================================
       TOKEN GENERATION (MIDNIGHT EXPIRY)
       ===================================================== */
    public String generateToken(
            UUID userId,
            String username,
            String role,
            UUID tokenId
    ) {
        Date now = new Date();
        Date expiryAtMidnight = computeNextMidnight();

        return Jwts.builder()
                .setClaims(Map.of(
                        "userId", userId.toString(),
                        "role", role,
                        "tokenId", tokenId.toString()
                ))
                .setSubject(username)
                .setIssuedAt(now)
                .setExpiration(expiryAtMidnight)
                .signWith(getSignKey(), SignatureAlgorithm.HS256)
                .compact();
    }

    /* =====================================================
       VALIDATION
       ===================================================== */
    public boolean validateToken(String token, String username) {
        return extractUsername(token).equals(username)
                && !isTokenExpired(token);
    }

    private boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    /* =====================================================
       EXTRACTORS
       ===================================================== */
    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public UUID extractUserId(String token) {
        return UUID.fromString(
                extractAllClaims(token).get("userId", String.class)
        );
    }

    public String extractUserRole(String token) {
        return extractAllClaims(token).get("role", String.class);
    }

    public UUID extractTokenId(String token) {
        return UUID.fromString(
                extractAllClaims(token).get("tokenId", String.class)
        );
    }

    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    public <T> T extractClaim(String token, Function<Claims, T> resolver) {
        return resolver.apply(extractAllClaims(token));
    }

    private Claims extractAllClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(getSignKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    /* =====================================================
       HELPERS
       ===================================================== */

    /**
     * Computes next midnight using SERVER timezone.
     * Server downtime does NOT matter — exp is embedded.
     */
    private Date computeNextMidnight() {
        ZoneId zone = ZoneId.systemDefault();
        LocalDate tomorrow = LocalDate.now(zone).plusDays(1);
        ZonedDateTime midnight = tomorrow.atStartOfDay(zone);
        return Date.from(midnight.toInstant());
    }

    /**
     * Used to set cookie Max-Age accurately.
     */
    public long secondsUntilMidnight() {
        ZonedDateTime now = ZonedDateTime.now();
        ZonedDateTime midnight =
                now.toLocalDate().plusDays(1).atStartOfDay(now.getZone());
        return Duration.between(now, midnight).getSeconds();
    }

    private Key getSignKey() {
        byte[] decodedKey = Decoders.BASE64.decode(secretKey);
        return Keys.hmacShaKeyFor(decodedKey);
    }
}