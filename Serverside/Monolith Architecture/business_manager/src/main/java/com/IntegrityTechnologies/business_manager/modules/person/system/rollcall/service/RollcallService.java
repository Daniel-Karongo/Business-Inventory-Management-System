package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallAudit;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.RollcallAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.RollcallRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class RollcallService {

    private final RollcallRepository rollcallRepository;
    private final RollcallAuditRepository rollcallAuditRepository;
    private final DepartmentRepository departmentRepository;
    private final UserRepository userRepository;
    private final BranchRepository branchRepository;

    @Value("${rollcall.default.start-time}")
    private String defaultStartTime;

    @Value("${rollcall.default.grace-minutes}")
    private int defaultGraceMinutes;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* =====================================================
       LOGIN ROLLCALL
       ===================================================== */

    @Transactional
    public RollcallDTO recordLoginRollcall(UUID userId, UUID departmentId, UUID branchId, RollcallMethod method) {

        User user = userRepository.findByIdAndTenantIdAndDeletedFalse(userId, TenantContext.getTenantId())
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByTenantIdAndIdAndDeletedFalse(
                        TenantContext.getTenantId(), departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchRepository.findByTenantIdAndIdAndDeletedFalse(
                TenantContext.getTenantId(), branchId)
                .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        validateDepartmentBranch(departmentId, branchId);

        LocalDate today = LocalDate.now();
        LocalDateTime now = LocalDateTime.now();

        // 1️⃣ Upgrade ABSENT → LATE if exists
        RollcallDTO upgraded =
                upgradeAbsentToLateIfExists(userId, departmentId, branchId, today, now);

        if (upgraded != null) {
            return upgraded;
        }

        // 2️⃣ Earliest LOGIN wins
        boolean alreadyExists =
                rollcallRepository
                        .findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethodNot(
                                userId, departmentId, branchId, today, RollcallMethod.LOGOUT
                        )
                        .isPresent();

        if (alreadyExists) {
            return rollcallRepository
                    .findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethodNot(
                            userId, departmentId, branchId, today, RollcallMethod.LOGOUT
                    )
                    .map(RollcallDTO::from)
                    .orElse(null);
        }


        RollcallStatus status = computeStatus(branch, dept, now);

        Rollcall login = Rollcall.builder()
                .tenantId(TenantContext.getTenantId())
                .branchId(branch.getId())
                .userId(userId)
                .username(user.getUsername())
                .departmentId(dept != null ? dept.getId() : null)
                .departmentName(dept != null ? dept.getName() : null)
                .branchName(branch.getName())
                .timestamp(now)
                .rollcallDate(today)
                .status(status)
                .method(method) // 🔥 key change
                .performedBy("SYSTEM")
                .build();

        Rollcall saved = rollcallRepository.save(login);

        rollcallAuditRepository.save(
                RollcallAudit.builder()
                        .rollcallId(saved.getId())
                        .userId(userId)
                        .departmentId(departmentId)
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .authMethod(method)
                        .action("CREATE")
                        .reason("Login rollcall recorded")
                        .performedBy("SYSTEM")
                        .timestamp(now)
                        .build()
        );

        return RollcallDTO.from(saved);
    }

    /* =====================================================
       LOGOUT ROLLCALL
       ===================================================== */

    @Transactional
    public RollcallDTO recordLogoutRollcall(UUID userId, UUID departmentId, UUID branchId) {

        User user = userRepository.findByIdAndTenantIdAndDeletedFalse(userId, TenantContext.getTenantId())
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByTenantIdAndIdAndDeletedFalse(
                        TenantContext.getTenantId(), departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchRepository.findByTenantIdAndIdAndDeletedFalse(
                TenantContext.getTenantId(), branchId)
                .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        validateDepartmentBranch(departmentId, branchId);

        LocalDate today = LocalDate.now();
        LocalDateTime now = LocalDateTime.now();

        Optional<Rollcall> existing =
                rollcallRepository.findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
                        userId, departmentId, branchId, today, RollcallMethod.LOGOUT
                );

        if (existing.isPresent()) {
            // ✅ Latest LOGOUT wins
            Rollcall logout = existing.get();
            logout.setTimestamp(now);
            logout.setPerformedBy("SYSTEM");

            Rollcall saved = rollcallRepository.save(logout);

            rollcallAuditRepository.save(
                    RollcallAudit.builder()
                            .rollcallId(saved.getId())
                            .userId(userId)
                            .departmentId(departmentId)
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .authMethod(RollcallMethod.LOGOUT)
                            .action("MODIFY")
                            .reason("Logout time updated")
                            .performedBy("SYSTEM")
                            .timestamp(now)
                            .build()
            );

            return RollcallDTO.from(saved);
        }

        Rollcall logout = Rollcall.builder()
                .tenantId(tenantId())
                .branchId(branch.getId())
                .userId(userId)
                .username(user.getUsername())
                .departmentId(dept != null ? dept.getId() : null)
                .departmentName(dept != null ? dept.getName() : null)
                .branchName(branch.getName())
                .timestamp(now)
                .rollcallDate(today)
                .status(RollcallStatus.PRESENT)
                .method(RollcallMethod.LOGOUT)
                .performedBy("SYSTEM")
                .build();

        return RollcallDTO.from(rollcallRepository.save(logout));
    }

    /* =====================================================
       QUERY METHODS (USED BY CONTROLLER)
       ===================================================== */

    @Transactional(readOnly = true)
    public List<RollcallDTO> getRollcallsForUser(UUID userId, LocalDateTime from, LocalDateTime to) {
        return rollcallRepository
                .findByUserIdAndTimestampBetweenOrderByTimestampDesc(userId, from, to)
                .stream()
                .map(RollcallDTO::from)
                .toList();
    }

    @Transactional(readOnly = true)
    public List<RollcallDTO> getRollcallsForDepartment(UUID departmentId, LocalDateTime from, LocalDateTime to) {
        return rollcallRepository
                .findByDepartmentIdAndTimestampBetweenOrderByTimestampDesc(departmentId, from, to)
                .stream()
                .map(RollcallDTO::from)
                .toList();
    }

    @Transactional(readOnly = true)
    public List<RollcallDTO> getRollcallsForBranch(UUID branchId, LocalDateTime from, LocalDateTime to) {
        return rollcallRepository
                .findByBranchIdAndTimestampBetweenOrderByTimestampDesc(branchId, from, to)
                .stream()
                .map(RollcallDTO::from)
                .toList();
    }

    /* =====================================================
       HELPERS
       ===================================================== */

    private RollcallDTO upgradeAbsentToLateIfExists(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            LocalDate today,
            LocalDateTime now
    ) {
        Optional<Rollcall> absentOpt =
                rollcallRepository.findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndStatus(
                        userId, departmentId, branchId, today, RollcallStatus.ABSENT
                );

        if (absentOpt.isEmpty()) return null;

        // 🚫 If a LOGIN already exists, do NOT upgrade
        boolean hasLogin =
                rollcallRepository
                        .findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethodNot(
                                userId, departmentId, branchId, today, RollcallMethod.LOGOUT
                        )
                        .isPresent();

        if (hasLogin) {
            return null; // earliest LOGIN stays authoritative
        }

        Rollcall absent = absentOpt.get();

        absent.setStatus(RollcallStatus.LATE);
        absent.setTimestamp(now);
        absent.setPerformedBy("SYSTEM");
        // ❌ DO NOT touch method here

        Rollcall saved = rollcallRepository.save(absent);

        rollcallAuditRepository.save(
                RollcallAudit.builder()
                        .rollcallId(saved.getId())
                        .userId(userId)
                        .departmentId(departmentId)
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .authMethod(saved.getMethod())
                        .action("MODIFY")
                        .reason("ABSENT → LATE")
                        .performedBy("SYSTEM")
                        .timestamp(now)
                        .build()
        );

        return RollcallDTO.from(saved);
    }

    private RollcallStatus computeStatus(
            Branch branch,
            Department dept,
            LocalDateTime now
    ) {

        LocalTime startTime = null;
        Integer grace = null;

        // 1️⃣ Department override
        if (dept != null && dept.getRollcallStartTime() != null) {
            startTime = dept.getRollcallStartTime();
            grace = dept.getGracePeriodMinutes();
        }

        // 2️⃣ Branch default
        if (startTime == null && branch.getRollcallStartTime() != null) {
            startTime = branch.getRollcallStartTime();
            grace = branch.getRollcallGraceMinutes();
        }

        // 3️⃣ Application fallback
        if (startTime == null) {
            startTime = LocalTime.parse(defaultStartTime);
        }

        if (grace == null) {
            grace = defaultGraceMinutes;
        }

        LocalDateTime start = now.toLocalDate().atTime(startTime);

        return now.isBefore(start.plusMinutes(grace))
                ? RollcallStatus.PRESENT
                : RollcallStatus.LATE;
    }

    private void validateDepartmentBranch(UUID departmentId, UUID branchId) {

        if (departmentId == null) return;

        boolean valid = branchRepository.branchContainsDepartment(
                        TenantContext.getTenantId(),
                        branchId,
                        departmentId
                );

        if (!valid) {
            throw new IllegalArgumentException(
                    "Department does not belong to branch"
            );
        }
    }
}