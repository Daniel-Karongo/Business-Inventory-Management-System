package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service;

import com.IntegrityTechnologies.business_manager.exception.BiometricException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricType;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.service.BiometricService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.NotificationService;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto.RollcallDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class RollcallService {

    private final RollcallRepository rollcallRepository;
    private final RollcallAuditRepository rollcallAuditRepository;
    private final DepartmentRepository departmentRepository;
    private final UserRepository userRepository;
    private final BiometricService biometricService;
    private final NotificationService notificationService; // interface to send emails / push
    private final BranchRepository branchRepository;
    private final UserSessionRepository userSessionRepository;

    @Transactional
    public RollcallDTO recordBiometricRollcall(UUID userId, UUID departmentId, UUID branchId, BiometricType type, byte[] rawTemplate) {
        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = null;
        if (departmentId != null) {
            dept = departmentRepository.findByIdAndDeletedFalse(departmentId)
                    .orElseThrow(() -> new EntityNotFoundException("Department not found"));
        }

        Branch branch = null;
        if (branchId != null) {
            branch = branchRepository.findByIdAndDeletedFalse(branchId)
                    .orElseThrow(() -> new EntityNotFoundException("Branch not found"));
        }

        Optional<BiometricRecord> matched = biometricService.verify(userId, type, rawTemplate);

        if (matched.isEmpty()) {
            // Optionally attempt identify across users (kiosk)
            Optional<BiometricRecord> maybe = biometricService.identify(type, rawTemplate);
            if (maybe.isPresent()) {
                matched = maybe;
                user = userRepository.findById(matched.get().getUser().getId()).orElse(user);
            } else {
                // record failed biometric attempt audit and throw
                rollcallAuditRepository.save(RollcallAudit.builder()
                        .userId(userId)
                        .departmentId(departmentId)
                        .branchId(branchId)
                        .action("BIOMETRIC_VERIFY_FAILED")
                        .reason("Biometric did not match")
                        .performedBy("SYSTEM")
                        .timestamp(LocalDateTime.now())
                        .build());
                throw new BiometricException("Biometric verification failed");
            }
        }

        // compute status
        RollcallStatus status = computeStatusForDepartment(dept, LocalDateTime.now());

        Rollcall r = Rollcall.builder()
                .user(user)
                .department(dept)
                .branch(branch)
                .timestamp(LocalDateTime.now())
                .status(status)
                .method(RollcallMethod.BIOMETRIC)
                .biometricRecordId(matched.map(BiometricRecord::getId).orElse(null))
                .performedBy("SYSTEM")
                .build();

        Rollcall saved = rollcallRepository.save(r);

        rollcallAuditRepository.save(RollcallAudit.builder()
                .rollcallId(saved.getId())
                .userId(user.getId())
                .departmentId(dept.getId())
                .branchId(branch.getId())
                .action("RECORD")
                .reason("Biometric rollcall recorded: " + status)
                .performedBy("SYSTEM")
                .timestamp(LocalDateTime.now())
                .build());

        return RollcallDTO.from(saved);
    }

    @Transactional
    public RollcallDTO recordLoginRollcall(UUID userId, UUID departmentId, UUID branchId) {

        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByIdAndDeletedFalse(departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchId == null ? null :
                branchRepository.findByIdAndDeletedFalse(branchId)
                        .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        RollcallStatus status = computeStatusForDepartment(dept, LocalDateTime.now());
        Rollcall saved = rollcallRepository.save(
                Rollcall.builder()
                        .user(user)
                        .department(dept)
                        .branch(branch)
                        .timestamp(LocalDateTime.now())
                        .status(status)
                        .method(RollcallMethod.LOGIN)
                        .performedBy("SYSTEM")
                        .build()
        );
        return RollcallDTO.from(saved);
    }

    @Transactional
    public RollcallDTO recordLogoutRollcall(UUID userId, UUID departmentId, UUID branchId) {

        User user = userRepository.findByIdAndDeletedFalse(userId)
                .orElseThrow(() -> new EntityNotFoundException("User not found"));

        Department dept = departmentId == null ? null :
                departmentRepository.findByIdAndDeletedFalse(departmentId)
                        .orElseThrow(() -> new EntityNotFoundException("Department not found"));

        Branch branch = branchId == null ? null :
                branchRepository.findByIdAndDeletedFalse(branchId)
                        .orElseThrow(() -> new EntityNotFoundException("Branch not found"));

        Rollcall saved = rollcallRepository.save(
                Rollcall.builder()
                        .user(user)
                        .department(dept)
                        .branch(branch)
                        .timestamp(LocalDateTime.now())
                        .status(RollcallStatus.PRESENT)
                        .method(RollcallMethod.LOGOUT)
                        .performedBy("SYSTEM")
                        .build()
        );

        return RollcallDTO.from(saved);
    }

    private RollcallStatus computeStatusForDepartment(Department dept, LocalDateTime now) {
        if (dept == null || dept.getRollcallStartTime() == null)
            return RollcallStatus.PRESENT;

        LocalDateTime start = now.toLocalDate().atTime(dept.getRollcallStartTime());
        int grace = dept.getGracePeriodMinutes() == null ? 15 : dept.getGracePeriodMinutes();

        return now.isBefore(start.plusMinutes(grace)) ?
                RollcallStatus.PRESENT : RollcallStatus.LATE;
    }

    /**
     * Scheduled job to mark ABSENT for users who didn't rollcall by cutoff.
     * This will be called (for example) every day at department.rollcallStartTime + some buffer.
     */
    @Scheduled(cron = "${rollcall.absentees.mark.cron}") // example: 09:30 daily; you can schedule per-department by more advanced scheduler
    @Transactional
    public void markAbsenteesAndNotify() {
        List<Department> departments = departmentRepository.findAllActive();
        LocalDateTime now = LocalDateTime.now();

        for (Department d : departments) {
            LocalDateTime cutoff = LocalDateTime.of(now.toLocalDate(), d.getRollcallStartTime()).plusMinutes(d.getGracePeriodMinutes() == null ? 15 : d.getGracePeriodMinutes());

            // list department members (heads and members) - you may want a repository method to fetch all user ids in department
            List<User> members = departmentRepository.findAllUsersInDepartment(d.getId());

            for (User user : members) {
                // skip if user already has a rollcall today
                LocalDateTime dayStart = cutoff.toLocalDate().atStartOfDay();
                LocalDateTime dayEnd = dayStart.plusDays(1);
                boolean has = !rollcallRepository.findByUserIdAndTimestampBetweenOrderByTimestampDesc(user.getId(), dayStart, dayEnd).isEmpty();
                if (!has) {
                    for(Branch branch: branchRepository.findBranchesByUserId(user.getId())) {
                        Rollcall absent = Rollcall.builder()
                            .user(user)
                            .department(d)
                            .timestamp(cutoff)
                            .status(RollcallStatus.ABSENT)
                            .method(null)
                            .performedBy("system")
                            .build();
                        Rollcall saved = rollcallRepository.save(absent);
                        rollcallAuditRepository.save(RollcallAudit.builder()
                                .rollcallId(saved.getId())
                                .userId(user.getId())
                                .departmentId(d.getId())
                                .branchId(branch.getId())
                                .action("AUTO_ABSENT")
                                .reason("No rollcall by cutoff")
                                .performedBy("system")
                                .timestamp(LocalDateTime.now())
                                .build());
                        // send notification to heads
                        notificationService.notifyAbsent(d, user, saved);
                    }
                }
            }
        }
    }



    // query methods
    public List<RollcallDTO> getRollcallsForUser(UUID userId, LocalDateTime from, LocalDateTime to) {
        List<Rollcall> rollcalls = rollcallRepository.findByUserIdAndTimestampBetweenOrderByTimestampDesc(userId, from, to);
        return rollcalls.stream().map(rollcall -> RollcallDTO.from(rollcall)).toList();
    }

    public List<RollcallDTO> getRollcallsForDepartment(UUID deptId, LocalDateTime from, LocalDateTime to) {
        List<Rollcall> rollcalls = rollcallRepository.findByDepartmentIdAndTimestampBetweenOrderByTimestampDesc(deptId, from, to);
        return rollcalls.stream().map(rollcall -> RollcallDTO.from(rollcall)).toList();
    }

    public List<RollcallDTO> getRollcallsForBranch(UUID branchId, LocalDateTime from, LocalDateTime to) {
        List<Rollcall> rollcalls = rollcallRepository.findByBranchIdAndTimestampBetweenOrderByTimestampDesc(branchId, from, to);
        return rollcalls.stream().map(rollcall -> RollcallDTO.from(rollcall)).toList();
    }
}

