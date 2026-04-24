package com.IntegrityTechnologies.business_manager.exception;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureException;
import jakarta.persistence.OptimisticLockException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authorization.AuthorizationDeniedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.resource.NoResourceFoundException;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

@RestControllerAdvice
public class GlobalExceptionHandler {

    /* ====================== AUTHENTICATION & JWT ====================== */

    @ExceptionHandler(AppSecurityException.class)
    public ResponseEntity<?> handleSecurity(AppSecurityException ex) {

        HttpStatus status = resolveStatus(ex.getCode());

        return ResponseEntity.status(status).body(Map.of(
                "error", status.getReasonPhrase(),
                "code", ex.getCode().name(),
                "message", ex.getMessage(),
                "status", status.value(),
                "timestamp", System.currentTimeMillis()
        ));
    }

    private HttpStatus resolveStatus(SecurityErrorCode code) {
        return switch (code) {

            // AUTH
            case INVALID_CREDENTIALS,
                    USER_NOT_FOUND -> HttpStatus.UNAUTHORIZED;

            case ACCOUNT_DISABLED,
                    ACCOUNT_DELETED,
                    PASSWORD_CHANGE_REQUIRED -> HttpStatus.FORBIDDEN;
            case SESSION_EXPIRED -> HttpStatus.UNAUTHORIZED;

            // DEVICE
            case DEVICE_PENDING_APPROVAL,
                    DEVICE_NOT_APPROVED,
                    DEVICE_NOT_REGISTERED,
                    DEVICE_ID_REQUIRED -> HttpStatus.FORBIDDEN;
            case DEVICE_NOT_FOUND,
                    PASSWORD_RESET_INVALID -> HttpStatus.BAD_REQUEST;

            case DEVICE_LIMIT_REACHED -> HttpStatus.TOO_MANY_REQUESTS;

            // LOCATION
            case LOCATION_REQUIRED,
                    LOCATION_OUTSIDE_BOUNDARY,
                    LOCATION_ACCURACY_LOW,
                    LOCATION_NOT_CONFIGURED -> HttpStatus.FORBIDDEN;

            // ACCESS
            case USER_NOT_IN_BRANCH,
                    BRANCH_NOT_FOUND -> HttpStatus.FORBIDDEN;

            // BIOMETRIC
            case BIOMETRIC_INVALID_PAYLOAD,
                    BIOMETRIC_VERIFICATION_FAILED,
                    BIOMETRIC_CREDENTIAL_NOT_FOUND,
                    BIOMETRIC_DEVICE_MISMATCH,
                    BIOMETRIC_TENANT_MISMATCH,
                    BIOMETRIC_USER_MISMATCH,
                    BIOMETRIC_CHALLENGE_EXPIRED -> HttpStatus.UNAUTHORIZED;

            // DEFAULT
            case INVALID_REQUEST,
                    UNKNOWN -> HttpStatus.BAD_REQUEST;

            default -> HttpStatus.BAD_REQUEST; // ✅ prevents future breakage
        };
    }

    @ExceptionHandler(BadCredentialsException.class)
    public ResponseEntity<?> handleBadCredentials(BadCredentialsException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "Unauthorized",
                "code", SecurityErrorCode.INVALID_CREDENTIALS.name(),
                "message", "Invalid username, password or branch",
                "status", 401,
                "timestamp", System.currentTimeMillis()
        ));
    }

    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<?> handleAuthenticationException(AuthenticationException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "Unauthorized",
                "code", SecurityErrorCode.INVALID_CREDENTIALS.name(),
                "message", "Authentication failed",
                "status", 401,
                "timestamp", System.currentTimeMillis()
        ));
    }

    @ExceptionHandler(ExpiredJwtException.class)
    public ResponseEntity<?> handleExpiredJwt(ExpiredJwtException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "Unauthorized",
                "code", SecurityErrorCode.SESSION_EXPIRED.name(),
                "message", "Session expired. Please log in again.",
                "status", 401,
                "timestamp", System.currentTimeMillis()
        ));
    }

    @ExceptionHandler(SignatureException.class)
    public ResponseEntity<?> handleSignatureException(SignatureException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "Unauthorized",
                "code", SecurityErrorCode.SESSION_EXPIRED.name(),
                "message", "Invalid session token",
                "status", 401,
                "timestamp", System.currentTimeMillis()
        ));
    }

    @ExceptionHandler(MalformedJwtException.class)
    public ResponseEntity<?> handleMalformedJwt(MalformedJwtException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                "error", "Bad Request",
                "code", SecurityErrorCode.INVALID_REQUEST.name(),
                "message", "Invalid token format",
                "status", 400,
                "timestamp", System.currentTimeMillis()
        ));
    }

    @ExceptionHandler(InvalidTokenException.class)
    public ResponseEntity<?> handleInvalidToken(InvalidTokenException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "Unauthorized",
                "code", SecurityErrorCode.SESSION_EXPIRED.name(),
                "message", ex.getMessage(),
                "status", 401,
                "timestamp", System.currentTimeMillis()
        ));
    }

    /* ====================== VALIDATION ERRORS ====================== */

    @ExceptionHandler(UnauthorizedAccessException.class)
    public ResponseEntity<Map<String, Object>> handleUnauthorizedAccess(UnauthorizedAccessException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(AuthorizationDeniedException.class)
    public ResponseEntity<Map<String, Object>> handleAuthorizationDenied(AuthorizationDeniedException ex) {
        Map<String, Object> body = new HashMap<>();
        body.put("status", HttpStatus.FORBIDDEN.value());
        body.put("error", HttpStatus.FORBIDDEN.getReasonPhrase());
        body.put("message", "You do not have enough privileges to access this resource(s)");
        body.put("timestamp", System.currentTimeMillis());
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(body);
    }

    @ExceptionHandler(ImageAccessDeniedException.class)
    public ResponseEntity<?> handleImageAccessDenied(ImageAccessDeniedException ex) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(ex.getMessage());
    }

    /* ====================== DOMAIN / ENTITY NOT FOUND ====================== */

    @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
    public ResponseEntity<Map<String, Object>> handleMediaTypeNotSupported(HttpMediaTypeNotSupportedException ex) {
        String supported = ex.getSupportedMediaTypes().isEmpty()
                ? "none"
                : ex.getSupportedMediaTypes().toString();

        String message = String.format(
                "Unsupported content type '%s'. Expected one of: %s",
                ex.getContentType(),
                supported
        );

        Map<String, Object> body = new HashMap<>();
        body.put("status", HttpStatus.UNSUPPORTED_MEDIA_TYPE.value());
        body.put("error", HttpStatus.UNSUPPORTED_MEDIA_TYPE.getReasonPhrase());
        body.put("message", message);
        body.put("timestamp", System.currentTimeMillis());
        return ResponseEntity.status(HttpStatus.UNSUPPORTED_MEDIA_TYPE).body(body);
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ApiResponse> handleConflict(DataIntegrityViolationException ex) {
        return ResponseEntity.status(HttpStatus.CONFLICT).body(new ApiResponse("CONFLICT", "Data integrity violation"));
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ApiResponse> handleBadRequest(IllegalArgumentException ex) {
        return ResponseEntity.badRequest().body(new ApiResponse("BAD_REQUEST", ex.getMessage()));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ApiResponse> handleValidation(MethodArgumentNotValidException ex) {
        String msg = ex.getBindingResult().getFieldErrors().stream()
                .map(f -> f.getField() + ": " + f.getDefaultMessage())
                .collect(Collectors.joining("; "));
        return ResponseEntity.badRequest().body(new ApiResponse("VALIDATION_ERROR", msg));
    }

    @ExceptionHandler(InvalidUserDataException.class)
    public ResponseEntity<Map<String, Object>> handleInvalidUserData(InvalidUserDataException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<Map<String, Object>> handleResponseStatusException(ResponseStatusException ex) {

        HttpStatus status = HttpStatus.valueOf(ex.getStatusCode().value());

        Map<String, Object> body = new HashMap<>();
        body.put("status", status.value());
        body.put("error", status.getReasonPhrase());
        body.put("message", ex.getReason());
        body.put("timestamp", System.currentTimeMillis());

        return new ResponseEntity<>(body, status);
    }
    @ExceptionHandler(DirectoryNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleUserImageDirectoryNotFound(DirectoryNotFoundException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(UserNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleUserNotFound(UserNotFoundException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(CategoryNotFoundException.class)
    public ResponseEntity<ApiResponse> handleCategoryNotFound(CategoryNotFoundException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new ApiResponse("INVALID_CATEGORY", ex.getMessage()));
    }

    @ExceptionHandler(ProductNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleProductNotFound(ProductNotFoundException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleEntityNotFound(EntityNotFoundException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(ImageNotFoundException.class)
    public ResponseEntity<?> handleImageNotFound(ImageNotFoundException ex) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(ex.getMessage());
    }

    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<String> handleIllegalState(IllegalStateException ex) {
        return ResponseEntity.badRequest().body(ex.getMessage());
    }

    /* ====================== BUSINESS LOGIC ERRORS ====================== */

    @ExceptionHandler(StorageFullException.class)
    public ResponseEntity<Map<String, Object>> handleStorageFullException(StorageFullException ex) {
        Map<String, Object> response = new HashMap<>();
        response.put("error", "Storage Full");
        response.put("message", ex.getMessage());
        response.put("status", 507); // 507 Insufficient Storage
        response.put("timestamp", System.currentTimeMillis());
        return ResponseEntity.status(507).body(response);
    }

    @ExceptionHandler(OutOfStockException.class)
    public ResponseEntity<Map<String, Object>> handleOutOfStock(OutOfStockException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.CONFLICT);
    }

    @ExceptionHandler(AccountingException.class)
    public ResponseEntity<Map<String, Object>> handleAccountingExceptions(AccountingException ex) {
        return buildResponse(ex.getMessage(), HttpStatus.CONFLICT);
    }

    @ExceptionHandler({
            ConcurrentUpdateException.class,
            OptimisticLockException.class,
            ObjectOptimisticLockingFailureException.class,
            OptimisticLockingFailureException.class
    })
    public ResponseEntity<ApiResponse>
    handleOptimisticLockConflict(
            Exception ex,
            WebRequest request
    ){

        return ResponseEntity
                .status(HttpStatus.CONFLICT)
                .body(
                        new ApiResponse(
                                "CONCURRENT_UPDATE",
                                "Another user/process updated this record moments ago. Please retry.",
                                null
                        )
                );

    }

    /* ====================== FALLBACK HANDLER ====================== */

    @ExceptionHandler(NoResourceFoundException.class)
    public ResponseEntity<Void> handleNoResourceFound(NoResourceFoundException ex) {
        return ResponseEntity.notFound().build();
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<?> handleGenericException(Exception ex, WebRequest request) {

        String path = request.getDescription(false);

        if (path != null && !path.contains("/api/")) {
            // Let Spring handle static resource errors normally
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }

        ex.printStackTrace(); // ✅ ADD

        return buildResponse(
                ex.getMessage(), // ✅ NO WRAPPING
                HttpStatus.INTERNAL_SERVER_ERROR
        );
    }

    /* ====================== HELPER METHOD ====================== */

    private ResponseEntity<Map<String, Object>> buildResponse(
            String message,
            HttpStatus status
    ) {
        return buildResponse(message, status, SecurityErrorCode.UNKNOWN);
    }

    private ResponseEntity<Map<String, Object>> buildResponse(
            String message,
            HttpStatus status,
            SecurityErrorCode code
    ) {
        Map<String, Object> body = new HashMap<>();
        body.put("status", status.value());
        body.put("error", status.getReasonPhrase());
        body.put("code", code.name());
        body.put("message", message);
        body.put("timestamp", System.currentTimeMillis());
        return new ResponseEntity<>(body, status);
    }
}