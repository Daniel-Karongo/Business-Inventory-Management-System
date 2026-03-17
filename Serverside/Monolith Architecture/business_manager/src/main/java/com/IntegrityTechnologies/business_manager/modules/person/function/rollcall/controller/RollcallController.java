package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.controller;

import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto.LoginRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
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
@TenantUserOnly
public class RollcallController {

    private final RollcallService rollcallService;
    private final PrivilegesChecker privilegesChecker;

    /* ====================================
       BIOMETRIC ROLLCALL
       ==================================== */

    @PostMapping("/biometric")
    public ResponseEntity<RollcallDTO> biometricRollcall(
            @RequestBody BiometricRollcallRequest req
    ) {

        byte[] template =
                Base64.getDecoder().decode(req.getTemplateBase64());

        RollcallDTO r =
                rollcallService.recordBiometricRollcall(
                        req.getUserId(),
                        req.getDepartmentId(),
                        req.getBranchId(),
                        req.getType(),
                        template
                );

        return ResponseEntity.ok(r);
    }

    /* ====================================
       LOGIN ROLLCALL
       ==================================== */

    @PostMapping("/login")
    public ResponseEntity<RollcallDTO> loginRollcall(
            @RequestBody LoginRollcallRequest req
    ) {

        RollcallDTO r =
                rollcallService.recordLoginRollcall(
                        req.getUserId(),
                        req.getDepartmentId(),
                        req.getBranchId()
                );

        return ResponseEntity.ok(r);
    }

    /* ====================================
       ABSENTEES
       ==================================== */

    @TenantManagerOnly
    @PostMapping("/mark-absentees")
    public ResponseEntity<List<RollcallDTO>> markAbsenteesManually() {

        return ResponseEntity.ok(
                rollcallService.markAbsenteesAndReturn()
        );
    }

    /* ====================================
       USER ROLLCALL
       ==================================== */

    @GetMapping("/user/{userId}")
    public ResponseEntity<List<RollcallDTO>> getUserRollcalls(
            @PathVariable UUID userId,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {

        LocalDateTime f =
                from == null
                        ? LocalDateTime.now().minusMonths(1)
                        : from.atStartOfDay();

        LocalDateTime t =
                to == null
                        ? LocalDateTime.now()
                        : to.atTime(LocalTime.MAX);

        return ResponseEntity.ok(
                rollcallService.getRollcallsForUser(userId, f, t)
        );
    }

    /* ====================================
       BRANCH ROLLCALL
       ==================================== */

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<List<RollcallDTO>> getBranchRollcalls(
            @PathVariable UUID branchId,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {

        LocalDateTime f =
                from == null
                        ? LocalDateTime.now().minusMonths(1)
                        : from.atStartOfDay();

        LocalDateTime t =
                to == null
                        ? LocalDateTime.now()
                        : to.atTime(LocalTime.MAX);

        return ResponseEntity.ok(
                rollcallService.getRollcallsForBranch(branchId, f, t)
        );
    }

    /* ====================================
       DEPARTMENT ROLLCALL
       ==================================== */

    @GetMapping("/department/{deptId}")
    public ResponseEntity<List<RollcallDTO>> getDeptRollcalls(
            @PathVariable UUID deptId,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {

        LocalDateTime f =
                from == null
                        ? LocalDateTime.now().minusMonths(1)
                        : from.atStartOfDay();

        LocalDateTime t =
                to == null
                        ? LocalDateTime.now()
                        : to.atTime(LocalTime.MAX);

        return ResponseEntity.ok(
                rollcallService.getRollcallsForDepartment(deptId, f, t)
        );
    }
}