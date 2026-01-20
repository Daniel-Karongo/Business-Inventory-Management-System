package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service;

import com.IntegrityTechnologies.business_manager.exception.BiometricException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.EmailService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricType;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.service.BiometricService;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
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
    private final BiometricService biometricService;
    private final EmailService notificationService;

    /* =====================================================
       LOGIN ROLLCALL
       ===================================================== */

    @Transactional
    public RollcallDTO recordLoginRollcall(UUID userId, UUID departmentId, UUID branchId) {

        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByIdAndDeletedFalse(departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        LocalDate today = LocalDate.now();
        LocalDateTime now = LocalDateTime.now();

        // 1️⃣ Upgrade ABSENT → LATE if exists
        RollcallDTO upgraded =
                upgradeAbsentToLateIfExists(userId, departmentId, branchId, today, now);

        if (upgraded != null) {
            return upgraded;
        }

        // 2️⃣ Earliest LOGIN wins
        Optional<Rollcall> existing =
                rollcallRepository.findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
                        userId, departmentId, branchId, today, RollcallMethod.LOGIN
                );

        if (existing.isPresent()) {
            return RollcallDTO.from(existing.get());
        }


        RollcallStatus status = computeStatusForDepartment(dept, now);

        Rollcall login = Rollcall.builder()
                .userId(userId)
                .username(user.getUsername())
                .departmentId(dept != null ? dept.getId() : null)
                .departmentName(dept != null ? dept.getName() : null)
                .branchId(branch.getId())
                .branchName(branch.getName())
                .timestamp(now)
                .rollcallDate(today)
                .status(status)
                .method(RollcallMethod.LOGIN)
                .performedBy("SYSTEM")
                .build();

        Rollcall saved = rollcallRepository.save(login);

        return RollcallDTO.from(saved);
    }

    /* =====================================================
       LOGOUT ROLLCALL
       ===================================================== */

    @Transactional
    public RollcallDTO recordLogoutRollcall(UUID userId, UUID departmentId, UUID branchId) {

        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByIdAndDeletedFalse(departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

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
                            .branchId(branchId)
                            .action("MODIFY")
                            .reason("Logout time updated")
                            .performedBy("SYSTEM")
                            .timestamp(now)
                            .build()
            );

            return RollcallDTO.from(saved);
        }

        Rollcall logout = Rollcall.builder()
                .userId(userId)
                .username(user.getUsername())
                .departmentId(dept != null ? dept.getId() : null)
                .departmentName(dept != null ? dept.getName() : null)
                .branchId(branch.getId())
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
       BIOMETRIC ROLLCALL
       ===================================================== */

    @Transactional
    public RollcallDTO recordBiometricRollcall(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            BiometricType type,
            byte[] rawTemplate
    ) {

        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentRepository.findByIdAndDeletedFalse(departmentId)
                .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        Optional<BiometricRecord> matched =
                biometricService.verify(userId, type, rawTemplate);

        if (matched.isEmpty()) {
            throw new BiometricException("Biometric verification failed");
        }

        LocalDate today = LocalDate.now();

        // ✅ BIOMETRIC first → LOGIN ignored
        boolean hasLogin =
                rollcallRepository.existsByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
                        userId, departmentId, branchId, today, RollcallMethod.LOGIN
                );

        if (hasLogin) {
            return RollcallDTO.from(
                    rollcallRepository
                            .findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
                                    userId, departmentId, branchId, today, RollcallMethod.LOGIN
                            ).orElseThrow()
            );
        }

        RollcallStatus status =
                computeStatusForDepartment(dept, LocalDateTime.now());

        RollcallDTO upgraded =
                upgradeAbsentToLateIfExists(userId, departmentId, branchId, today, LocalDateTime.now());

        if (upgraded != null) {
            return upgraded;
        }

        Rollcall biometric = Rollcall.builder()
                .userId(userId)
                .username(user.getUsername())
                .departmentId(dept.getId())
                .departmentName(dept.getName())
                .branchId(branch.getId())
                .branchName(branch.getName())
                .timestamp(LocalDateTime.now())
                .rollcallDate(today)
                .status(status)
                .method(RollcallMethod.BIOMETRIC)
                .biometricRecordId(matched.get().getId())
                .performedBy("SYSTEM")
                .build();

        return RollcallDTO.from(rollcallRepository.save(biometric));
    }

    /* =====================================================
       ABSENT MARKING (SCHEDULED)
       ===================================================== */

    @Transactional
    public List<RollcallDTO> markAbsenteesAndReturn() {

        List<RollcallDTO> created = new java.util.ArrayList<>();

        List<Department> departments = departmentRepository.findAllActive();
        LocalDate today = LocalDate.now();
        LocalDateTime now = LocalDateTime.now();

        for (Department d : departments) {
            List<User> users = departmentRepository.findAllUsersInDepartment(d.getId());

            for (User u : users) {
                for (Branch b : branchRepository.findBranchesByUserId(u.getId())) {

                    boolean alreadyHasRollcall =
                            rollcallRepository.existsByUserIdAndDepartmentIdAndBranchIdAndRollcallDate(
                                    u.getId(), d.getId(), b.getId(), today
                            );

                    if (alreadyHasRollcall) continue;

                    Rollcall absent = Rollcall.builder()
                            .userId(u.getId())
                            .username(u.getUsername())
                            .departmentId(d.getId())
                            .departmentName(d.getName())
                            .branchId(b.getId())
                            .branchName(b.getName())
                            .timestamp(now)
                            .rollcallDate(today)
                            .status(RollcallStatus.ABSENT)
                            .method(null)
                            .performedBy("SYSTEM")
                            .build();

                    Rollcall saved = rollcallRepository.save(absent);
//                    notificationService.notifyAbsent(d, u, saved);

                    created.add(RollcallDTO.from(saved));
                }
            }
        }

        return created;
    }

    @Scheduled(cron = "${rollcall.absentees.mark.cron}")
    @Transactional
    public void markAbsentees() {
        markAbsenteesAndReturn();
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
                rollcallRepository.existsByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
                        userId, departmentId, branchId, today, RollcallMethod.LOGIN
                );

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
                        .branchId(branchId)
                        .action("MODIFY")
                        .reason("ABSENT → LATE")
                        .performedBy("SYSTEM")
                        .timestamp(now)
                        .build()
        );

        return RollcallDTO.from(saved);
    }

    private RollcallStatus computeStatusForDepartment(Department dept, LocalDateTime now) {
        if (dept == null || dept.getRollcallStartTime() == null) {
            return RollcallStatus.PRESENT;
        }

        LocalDateTime start = now.toLocalDate().atTime(dept.getRollcallStartTime());
        int grace = dept.getGracePeriodMinutes() == null ? 15 : dept.getGracePeriodMinutes();

        return now.isBefore(start.plusMinutes(grace))
                ? RollcallStatus.PRESENT
                : RollcallStatus.LATE;
    }
}