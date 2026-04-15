package com.IntegrityTechnologies.business_manager.security.auth.tenant;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.common.AuthResponseFactory;
import com.IntegrityTechnologies.business_manager.security.auth.common.JwtFactory;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceUsageService;
import com.IntegrityTechnologies.business_manager.security.device.service.LocationSecurityService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantAuthService {

    private static final int TENANT_USERS_MAX_SESSIONS_PER_DAY = 5;

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final RollcallService rollcallService;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final UserSessionRepository userSessionRepository;
    private final LoginAuditService loginAuditService;
    private final DeviceSecurityService deviceSecurityService;
    private final LocationSecurityService locationSecurityService;
    private final DeviceUsageService deviceUsageService;
    private final JwtFactory jwtFactory;
    private final AuthResponseFactory responseFactory;
    private final TenantSessionService sessionService;

    public record Result(String jwt, AuthResponse response) {}

    public Result login(AuthRequest request, HttpServletRequest httpRequest) {

        UUID tenantId = TenantContext.getTenantId();
        UUID branchId = request.getBranchId();

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        String ip = extractClientIp(httpRequest);

        var branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(tenantId, branchId)
                .orElseThrow(() -> new BadCredentialsException("Branch not found"));

        try {
            deviceSecurityService.validate(tenantId, branchId, request.getDeviceId());

            locationSecurityService.validate(
                    branch,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy()
            );

        } catch (Exception ex) {

            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    request.getDeviceId(),
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    ex.getMessage()
            );

            throw new BadCredentialsException(ex.getMessage());
        }

        User user = userRepository
                .findAuthUser(tenantId, request.getIdentifier())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        UUID userId = user.getId();
        LocalDate today = LocalDate.now();

        boolean belongs = branchRepository.findBranchesByUserId(
                tenantId,
                userId
        ).stream().anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                        .size();

        if (activeCount >= TENANT_USERS_MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        UUID tokenId = UUID.randomUUID();

        String token = jwtFactory.generateTenantToken(
                tenantId,
                userId,
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                branchId,
                request.getDeviceId()
        );

        TrustedDevice device = deviceSecurityService
                .getByDeviceId(tenantId, branchId, request.getDeviceId());

        deviceUsageService.record(device.getId(), userId);
        sessionService.create(tenantId, branchId, userId, tokenId);

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId,
                        userId,
                        branchId
                );

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(
                    userId,
                    null,
                    branchId,
                    RollcallMethod.LOGIN_PASSWORD
            );
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(
                        userId,
                        d.getId(),
                        branchId,
                        RollcallMethod.LOGIN_PASSWORD
                );
            }
        }

        AuthResponse response = responseFactory.tenant(
                userId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList()
        );

        loginAuditService.log(
                tenantId,
                userId,
                branchId,
                request.getDeviceId(),
                request.getLatitude(),
                request.getLongitude(),
                request.getAccuracy(),
                ip,
                "SUCCESS",
                "OK"
        );

        return new Result(token, response);
    }

    private String extractClientIp(HttpServletRequest request) {
        String xf = request.getHeader("X-Forwarded-For");
        if (xf != null && !xf.isBlank()) {
            return xf.split(",")[0].trim();
        }
        return request.getRemoteAddr();
    }

    public Result biometricLogin(AuthRequest request, HttpServletRequest httpRequest) {

        UUID tenantId = TenantContext.getTenantId();
        UUID branchId = request.getBranchId();

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        String ip = extractClientIp(httpRequest);

        var branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(tenantId, branchId)
                .orElseThrow(() -> new BadCredentialsException("Branch not found"));

        try {
            deviceSecurityService.validate(tenantId, branchId, request.getDeviceId());

            locationSecurityService.validate(
                    branch,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy()
            );

        } catch (Exception ex) {

            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    request.getDeviceId(),
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    ex.getMessage()
            );

            throw new BadCredentialsException(ex.getMessage());
        }

        User user = userRepository
                .findByIdAndTenantId(request.getUserId(), tenantId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (Boolean.TRUE.equals(user.getDeleted())) {
            throw new BadCredentialsException("Account deleted");
        }

        UUID userId = user.getId();
        LocalDate today = LocalDate.now();

        boolean belongs = branchRepository.findBranchesByUserId(
                tenantId,
                userId
        ).stream().anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                        .size();

        if (activeCount >= TENANT_USERS_MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        UUID tokenId = UUID.randomUUID();

        String token = jwtFactory.generateTenantToken(
                tenantId,
                userId,
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                branchId,
                request.getDeviceId()
        );

        TrustedDevice device = deviceSecurityService
                .getByDeviceId(tenantId, branchId, request.getDeviceId());

        deviceUsageService.record(device.getId(), userId);

        sessionService.create(tenantId, branchId, userId, tokenId);

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId,
                        userId,
                        branchId
                );

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(
                    userId,
                    null,
                    branchId,
                    RollcallMethod.LOGIN_BIOMETRIC
            );
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(
                        userId,
                        d.getId(),
                        branchId,
                        RollcallMethod.LOGIN_BIOMETRIC
                );
            }
        }

        AuthResponse response = responseFactory.tenant(
                userId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList()
        );

        loginAuditService.log(
                tenantId,
                userId,
                branchId,
                request.getDeviceId(),
                request.getLatitude(),
                request.getLongitude(),
                request.getAccuracy(),
                ip,
                "SUCCESS",
                "OK"
        );

        return new Result(token, response);
    }
}