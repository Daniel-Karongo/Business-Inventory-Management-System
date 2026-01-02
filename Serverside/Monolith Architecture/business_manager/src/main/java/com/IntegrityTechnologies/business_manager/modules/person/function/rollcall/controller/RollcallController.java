package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.controller;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.LoginRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
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
    public ResponseEntity<RollcallDTO> biometricRollcall(@RequestBody BiometricRollcallRequest req) {
        // ensure user or kiosk has rights to record
        byte[] template = Base64.getDecoder().decode(req.getTemplateBase64());
        RollcallDTO r = rollcallService.recordBiometricRollcall(req.getUserId(), req.getDepartmentId(), req.getBranchId(), req.getType(), template);
        return ResponseEntity.ok(r);
    }

    // Login-based rollcall (user logs in)
    @PostMapping("/login")
    public ResponseEntity<RollcallDTO> loginRollcall(@RequestBody LoginRollcallRequest req) {
        RollcallDTO r = rollcallService.recordLoginRollcall(req.getUserId(), req.getDepartmentId(), req.getBranchId());
        return ResponseEntity.ok(r);
    }

    @PostMapping("/mark-absentees")
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    public ResponseEntity<List<RollcallDTO>> markAbsenteesManually() {
        return ResponseEntity.ok(rollcallService.markAbsenteesAndReturn());
    }

    @GetMapping("/user/{userId}")
    public ResponseEntity<List<RollcallDTO>> getUserRollcalls(
            @PathVariable UUID userId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to) {
        LocalDateTime f = from == null ? LocalDateTime.now().minusMonths(1) : from.atStartOfDay();
        LocalDateTime t = to == null ? LocalDateTime.now() : to.atTime(LocalTime.MAX);
        return ResponseEntity.ok(rollcallService.getRollcallsForUser(userId, f, t));
    }

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<List<RollcallDTO>> getBranchRollcalls(
            @PathVariable UUID branchId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {
        LocalDateTime f = from == null ? LocalDateTime.now().minusMonths(1) : from.atStartOfDay();
        LocalDateTime t = to == null ? LocalDateTime.now() : to.atTime(LocalTime.MAX);
        return ResponseEntity.ok(rollcallService.getRollcallsForBranch(branchId, f, t));
    }

    @GetMapping("/department/{deptId}")
    public ResponseEntity<List<RollcallDTO>> getDeptRollcalls(
            @PathVariable UUID deptId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {
        LocalDateTime f = from == null ? LocalDateTime.now().minusMonths(1) : from.atStartOfDay();
        LocalDateTime t = to == null ? LocalDateTime.now() : to.atTime(LocalTime.MAX);
        return ResponseEntity.ok(rollcallService.getRollcallsForDepartment(deptId, f, t));
    }
}