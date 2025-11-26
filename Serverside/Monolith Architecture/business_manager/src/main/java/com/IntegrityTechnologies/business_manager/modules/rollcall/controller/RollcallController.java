package com.IntegrityTechnologies.business_manager.modules.rollcall.controller;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.modules.biometric.model.BiometricRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.rollcall.model.LoginRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Base64;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/rollcall")
@RequiredArgsConstructor
@Tag(name = "Rollcall")
public class RollcallController {

    private final RollcallService rollcallService;
    private final PrivilegesChecker privilegesChecker;

    // Biometric rollcall (kiosk or mobile) - raw template bytes expected as base64
    @PostMapping("/biometric")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR','EMPLOYEE')")
    public ResponseEntity<Rollcall> biometricRollcall(@RequestBody BiometricRollcallRequest req, Authentication auth) {
        // ensure user or kiosk has rights to record
        String performedBy = SecurityUtils.currentUsername();
        byte[] template = Base64.getDecoder().decode(req.getTemplateBase64());
        Rollcall r = rollcallService.recordBiometricRollcall(req.getUserId(), req.getDepartmentId(), req.getType(), template, performedBy);
        return ResponseEntity.ok(r);
    }

    // Login-based rollcall (user logs in)
    @PostMapping("/login")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR','EMPLOYEE')")
    public ResponseEntity<Rollcall> loginRollcall(@RequestBody LoginRollcallRequest req) {
        String performedBy = SecurityUtils.currentUsername();
        Rollcall r = rollcallService.recordLoginRollcall(req.getUserId(), req.getDepartmentId(), performedBy);
        return ResponseEntity.ok(r);
    }

    @GetMapping("/user/{userId}")
    public ResponseEntity<List<Rollcall>> getUserRollcalls(@PathVariable UUID userId,
                                                           @RequestParam(required = false) String from,
                                                           @RequestParam(required = false) String to) {
        LocalDateTime f = from == null ? LocalDateTime.now().minusMonths(1) : LocalDateTime.parse(from);
        LocalDateTime t = to == null ? LocalDateTime.now() : LocalDateTime.parse(to);
        return ResponseEntity.ok(rollcallService.getRollcallsForUser(userId, f, t));
    }

    @GetMapping("/department/{deptId}")
    public ResponseEntity<List<Rollcall>> getDeptRollcalls(@PathVariable UUID deptId,
                                                           @RequestParam(required = false) String from,
                                                           @RequestParam(required = false) String to) {
        LocalDateTime f = from == null ? LocalDateTime.now().minusMonths(1) : LocalDateTime.parse(from);
        LocalDateTime t = to == null ? LocalDateTime.now() : LocalDateTime.parse(to);
        return ResponseEntity.ok(rollcallService.getRollcallsForDepartment(deptId, f, t));
    }
}