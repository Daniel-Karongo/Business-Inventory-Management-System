package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service;

import com.IntegrityTechnologies.business_manager.exception.BiometricException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricType;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.service.BiometricService;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.NotificationService;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallAudit;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.RollcallRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final BiometricService biometricService;
    private final NotificationService notificationService; // interface to send emails / push

    @Transactional
    public Rollcall recordBiometricRollcall(UUID userId, UUID departmentId, BiometricType type, byte[] rawTemplate) {
        User user = userRepository.findById(userId).orElseThrow(() -> new EntityNotFoundException("User not found"));
        Department dept = departmentRepository.findById(departmentId).orElseThrow(() -> new EntityNotFoundException("Department not found"));

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
                        .action("BIOMETRIC_VERIFY_FAILED")
                        .reason("Biometric did not match")
                        .performedBy(userId.toString())
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
                .timestamp(LocalDateTime.now())
                .status(status)
                .method(RollcallMethod.BIOMETRIC)
                .biometricRecordId(matched.map(BiometricRecord::getId).orElse(null))
                .performedBy(userId.toString())
                .build();

        Rollcall saved = rollcallRepository.save(r);

        rollcallAuditRepository.save(RollcallAudit.builder()
                .rollcallId(saved.getId())
                .userId(user.getId())
                .departmentId(dept.getId())
                .action("RECORD")
                .reason("Biometric rollcall recorded: " + status)
                .performedBy(userId.toString())
                .timestamp(LocalDateTime.now())
                .build());

        return saved;
    }

    @Transactional
    public Rollcall recordLoginRollcall(UUID userId, UUID departmentId) {
        User user = userRepository.findById(userId).orElseThrow(() -> new EntityNotFoundException("User not found"));
        Department dept = departmentRepository.findById(departmentId).orElseThrow(() -> new EntityNotFoundException("Department not found"));

        RollcallStatus status = computeStatusForDepartment(dept, LocalDateTime.now());

        Rollcall r = Rollcall.builder()
                .user(user)
                .department(dept)
                .timestamp(LocalDateTime.now())
                .status(status)
                .method(RollcallMethod.LOGIN)
                .performedBy(user.getUsername())
                .build();

        Rollcall saved = rollcallRepository.save(r);

        rollcallAuditRepository.save(RollcallAudit.builder()
                .rollcallId(saved.getId())
                .userId(user.getId())
                .departmentId(dept.getId())
                .action("RECORD")
                .reason("Login rollcall recorded: " + status)
                .performedBy(user.getUsername())
                .timestamp(LocalDateTime.now())
                .build());

        return saved;
    }

    private RollcallStatus computeStatusForDepartment(Department dept, LocalDateTime now) {
        if (dept == null || dept.getRollcallStartTime() == null) return RollcallStatus.PRESENT;

        LocalTime start = dept.getRollcallStartTime();
        int grace = dept.getGracePeriodMinutes() == null ? 15 : dept.getGracePeriodMinutes();

        LocalDateTime startDateTime = LocalDateTime.of(now.toLocalDate(), start);
        if (now.isBefore(startDateTime.plusMinutes(grace))) return RollcallStatus.PRESENT;
        return RollcallStatus.LATE;
    }

    /**
     * Scheduled job to mark ABSENT for users who didn't rollcall by cutoff.
     * This will be called (for example) every day at department.rollcallStartTime + some buffer.
     */
    @Scheduled(cron = "0 30 9 * * ?") // example: 09:30 daily; you can schedule per-department by more advanced scheduler
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
                boolean has = !rollcallRepository.findByUserIdAndTimestampBetween(user.getId(), dayStart, dayEnd).isEmpty();
                if (!has) {
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

    // query methods
    public List<Rollcall> getRollcallsForUser(UUID userId, LocalDateTime from, LocalDateTime to) {
        return rollcallRepository.findByUserIdAndTimestampBetween(userId, from, to);
    }

    public List<Rollcall> getRollcallsForDepartment(UUID deptId, LocalDateTime from, LocalDateTime to) {
        return rollcallRepository.findByDepartmentIdAndTimestampBetween(deptId, from, to);
    }
}

