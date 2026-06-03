package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.controller;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/rollcall")
@RequiredArgsConstructor
@Tag(name = "Rollcall")
@TenantUserOnly
public class RollcallController {

    private final RollcallService rollcallService;

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
}