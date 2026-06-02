package com.IntegrityTechnologies.business_manager.modules.person.branch.service;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettingsSeeder;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.AccountingPeriodBootstrapService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.branch.mapper.BranchMapper;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.specification.BranchSpecification;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserBranchId;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserBranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionGuard;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BranchService {

    private final BranchRepository branchRepository;
    private final BranchAuditRepository branchAuditRepository;

    private final UserRepository userRepository;
    private final DepartmentRepository departmentRepository;

    private final UserBranchRepository userBranchRepository;
    private final UserDepartmentRepository userDepartmentRepository;

    private final BranchAccountingSettingsRepository branchAccountingSettingsRepository;

    private final BranchChartOfAccountsService coaService;

    private final PrivilegesChecker privilegesChecker;

    private final SubscriptionGuard subscriptionGuard;

    private final TenantMetadataCache tenantMetadataCache;

    private final TaxSystemStateRepository taxSystemStateRepository;

    private final TaxProperties taxProperties;

    private final AccountingPeriodBootstrapService accountingPeriodBootstrapService;

    private final BranchNotificationSettingsSeeder notificationSettingsSeeder;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* =====================================================
       CREATE
    ===================================================== */

    @Transactional
    public BranchDetailsDTO create(
            BranchFormDTO request,
            Authentication authentication
    ) {

        subscriptionGuard.checkBranchLimit();

        if (
                branchRepository.existsByTenantIdAndBranchCodeIgnoreCase(
                        tenantId(),
                        request.getBranchCode()
                )
        ) {
            throw new IllegalArgumentException(
                    "Branch code already exists: " + request.getBranchCode()
            );
        }

        Branch branch = Branch.builder()
                .tenantId(tenantId())
                .branchCode(request.getBranchCode().trim().toUpperCase())
                .name(request.getName())
                .location(request.getLocation())
                .phone(
                        PhoneAndEmailNormalizer.normalizePhone(
                                request.getPhone()
                        )
                )
                .email(request.getEmail())
                .latitude(request.getLatitude())
                .longitude(request.getLongitude())
                .radiusMeters(request.getRadiusMeters())
                .enforceGeofence(Boolean.TRUE.equals(request.getEnforceGeofence()))
                .enforceDevice(Boolean.TRUE.equals(request.getEnforceDevice()))
                .rollcallStartTime(request.getRollcallStartTime())
                .rollcallGraceMinutes(request.getRollcallGraceMinutes())
                .logoutTime(request.getLogoutTime())
                .deleted(false)
                .build();

        branch = branchRepository.save(branch);

        bootstrapAccounting(branch);

        notificationSettingsSeeder.seed(
                tenantId(),
                branch.getId()
        );

        assignUsers(branch, request.getUserIds());

        updateDepartments(
                branch,
                request.getDepartmentIds()
        );

        recordAudit(
                branch,
                "CREATE",
                null,
                null,
                null,
                authentication,
                "Branch created"
        );

        invalidateBranchCacheAfterCommit();

        return getById(branch.getId());
    }

    /* =====================================================
       READ
    ===================================================== */

    @Transactional(readOnly = true)
    public PageWrapper<BranchListItemDTO> getAll(
            BranchQueryDTO query
    ) {

        Sort.Direction direction =
                "asc".equalsIgnoreCase(
                        query.getSortDirection()
                )
                        ? Sort.Direction.ASC
                        : Sort.Direction.DESC;

        PageRequest pageable =
                PageRequest.of(
                        query.getPage(),
                        query.getSize(),
                        Sort.by(
                                direction,
                                query.getSortBy()
                        )
                );

        Page<Branch> page =
                branchRepository.findAll(
                        BranchSpecification.build(
                                tenantId(),
                                query
                        ),
                        pageable
                );

        Page<BranchListItemDTO> dtoPage =
                page.map(
                        BranchMapper::toListItemDTO
                );

        return new PageWrapper<>(dtoPage);
    }

    @Transactional(readOnly = true)
    public BranchDetailsDTO getById(
            UUID id
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndId(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Branch not found: " + id
                        )
                );

        Set<User> users =
                userBranchRepository
                        .findByBranchId(
                                tenantId(),
                                branch.getId()
                        )
                        .stream()
                        .map(UserBranch::getUser)
                        .collect(Collectors.toSet());

        Set<Department> departments =
                departmentRepository
                        .findByTenantIdAndBranch_Id(
                                tenantId(),
                                branch.getId()
                        )
                        .stream()
                        .collect(Collectors.toSet());

        return BranchMapper.toDetailsDTO(
                branch,
                users,
                departments
        );
    }

    /* =====================================================
       UPDATE
    ===================================================== */

    @Transactional
    public BranchDetailsDTO update(
            UUID id,
            BranchFormDTO request,
            Authentication authentication
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Branch not found: " + id
                        )
                );

        updateField(
                branch,
                "name",
                branch.getName(),
                request.getName(),
                branch::setName,
                authentication
        );

        updateField(
                branch,
                "location",
                branch.getLocation(),
                request.getLocation(),
                branch::setLocation,
                authentication
        );

        String normalizedPhone =
                PhoneAndEmailNormalizer.normalizePhone(
                        request.getPhone()
                );

        updateField(
                branch,
                "phone",
                branch.getPhone(),
                normalizedPhone,
                branch::setPhone,
                authentication
        );

        updateField(
                branch,
                "email",
                branch.getEmail(),
                request.getEmail(),
                branch::setEmail,
                authentication
        );

        branch.setLatitude(request.getLatitude());
        branch.setLongitude(request.getLongitude());
        branch.setRadiusMeters(request.getRadiusMeters());

        branch.setEnforceGeofence(
                Boolean.TRUE.equals(request.getEnforceGeofence())
        );

        branch.setEnforceDevice(
                Boolean.TRUE.equals(request.getEnforceDevice())
        );

        branch.setRollcallStartTime(
                request.getRollcallStartTime()
        );

        branch.setRollcallGraceMinutes(
                request.getRollcallGraceMinutes()
        );

        branch.setLogoutTime(
                request.getLogoutTime()
        );

        updateUsers(
                branch,
                request.getUserIds()
        );

        updateDepartments(
                branch,
                request.getDepartmentIds()
        );

        branchRepository.save(branch);

        invalidateBranchCacheAfterCommit();

        return getById(branch.getId());
    }

        /* =====================================================
       ATTENDANCE SETTINGS
    ===================================================== */

    @Transactional
    public BranchDetailsDTO updateAttendanceSettings(
            UUID id,
            BranchAttendanceSettingsUpdateDTO request,
            Authentication authentication
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Branch not found: " + id
                        )
                );

        validateAttendanceSettings(request);

        updateAttendanceField(
                branch,
                "rollcallStartTime",
                branch.getRollcallStartTime(),
                request.getRollcallStartTime(),
                branch::setRollcallStartTime,
                authentication
        );

        updateAttendanceField(
                branch,
                "rollcallGraceMinutes",
                branch.getRollcallGraceMinutes(),
                request.getRollcallGraceMinutes(),
                branch::setRollcallGraceMinutes,
                authentication
        );

        updateAttendanceField(
                branch,
                "logoutTime",
                branch.getLogoutTime(),
                request.getLogoutTime(),
                branch::setLogoutTime,
                authentication
        );

        branchRepository.save(branch);

        invalidateBranchCacheAfterCommit();

        return getById(branch.getId());
    }

    /* =====================================================
       SECURITY SETTINGS
    ===================================================== */

    @Transactional
    public BranchDetailsDTO updateSecuritySettings(
            UUID id,
            BranchSecuritySettingsUpdateDTO request,
            Authentication authentication
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Branch not found: " + id
                        )
                );

        validateSecuritySettings(request);

        updateSecurityField(
                branch,
                "latitude",
                branch.getLatitude(),
                request.getLatitude(),
                branch::setLatitude,
                authentication
        );

        updateSecurityField(
                branch,
                "longitude",
                branch.getLongitude(),
                request.getLongitude(),
                branch::setLongitude,
                authentication
        );

        updateSecurityField(
                branch,
                "radiusMeters",
                branch.getRadiusMeters(),
                request.getRadiusMeters(),
                branch::setRadiusMeters,
                authentication
        );

        updateSecurityField(
                branch,
                "enforceGeofence",
                branch.getEnforceGeofence(),
                request.getEnforceGeofence(),
                branch::setEnforceGeofence,
                authentication
        );

        updateSecurityField(
                branch,
                "enforceDevice",
                branch.getEnforceDevice(),
                request.getEnforceDevice(),
                branch::setEnforceDevice,
                authentication
        );

        branchRepository.save(branch);

        invalidateBranchCacheAfterCommit();

        return getById(branch.getId());
    }

    /* =====================================================
       DELETE
    ===================================================== */

    @Transactional
    public void deleteBranch(
            UUID id,
            Boolean soft,
            Authentication authentication
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndId(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Branch not found: " + id
                        )
                );

        if (Boolean.TRUE.equals(soft)) {

            branch.setDeleted(true);

            branchRepository.save(branch);

            recordAudit(
                    branch,
                    "SOFT_DELETE",
                    "deleted",
                    "false",
                    "true",
                    authentication,
                    "Branch soft deleted"
            );

        } else {

            userBranchRepository.deleteByBranchId(
                    tenantId(),
                    id
            );

            departmentRepository
                    .findByTenantIdAndBranch_Id(
                            tenantId(),
                            id
                    )
                    .forEach(
                            d -> userDepartmentRepository
                                    .deleteByDepartmentId(
                                            tenantId(),
                                            d.getId()
                                    )
                    );

            branchRepository.delete(branch);

            recordAudit(
                    branch,
                    "HARD_DELETE",
                    null,
                    branch.toString(),
                    null,
                    authentication,
                    "Branch permanently deleted"
            );
        }

        invalidateBranchCacheAfterCommit();
    }

    /* =====================================================
       RESTORE
    ===================================================== */

    @Transactional
    public void restoreBranch(
            UUID id,
            Authentication authentication
    ) {

        Branch branch = branchRepository
                .findByTenantIdAndIdAndDeletedTrue(
                        tenantId(),
                        id
                )
                .orElseThrow(
                        () -> new EntityNotFoundException(
                                "Deleted branch not found: " + id
                        )
                );

        branch.setDeleted(false);

        branchRepository.save(branch);

        recordAudit(
                branch,
                "RESTORE",
                "deleted",
                "true",
                "false",
                authentication,
                "Branch restored"
        );

        invalidateBranchCacheAfterCommit();
    }

    /* =====================================================
       INTERNAL
    ===================================================== */

    private void bootstrapAccounting(
            Branch branch
    ) {

        branchAccountingSettingsRepository.save(
                BranchAccountingSettings.builder()
                        .branchId(branch.getId())
                        .tenantId(tenantId())
                        .revenueRecognitionMode(
                                RevenueRecognitionMode.DELIVERY
                        )
                        .build()
        );

        coaService.seedForBranch(
                tenantId(),
                branch.getId()
        );

        accountingPeriodBootstrapService.ensurePeriods(
                tenantId(),
                branch.getId()
        );

        if (
                taxSystemStateRepository
                        .findByTenantIdAndBranchId(
                                tenantId(),
                                branch.getId()
                        )
                        .isEmpty()
        ) {

            taxSystemStateRepository.save(
                    TaxSystemState.builder()
                            .tenantId(tenantId())
                            .branchId(branch.getId())
                            .taxMode(
                                    taxProperties.getBusinessTaxMode()
                            )
                            .vatEnabled(
                                    taxProperties.isVatEnabled()
                            )
                            .vatRate(
                                    taxProperties.getVatRate()
                            )
                            .corporateTaxRate(
                                    taxProperties.getCorporateTaxRate()
                            )
                            .locked(false)
                            .build()
            );
        }
    }

    private void assignUsers(
            Branch branch,
            List<UUID> userIds
    ) {

        if (userIds == null || userIds.isEmpty()) {
            return;
        }

        List<User> users =
                userRepository.findAllByTenantIdAndIdIn(
                        tenantId(),
                        userIds
                );

        for (User user : users) {

            UserBranch relation =
                    UserBranch.builder()
                            .id(
                                    new UserBranchId(
                                            user.getId(),
                                            branch.getId()
                                    )
                            )
                            .user(user)
                            .branch(branch)
                            .primaryBranch(false)
                            .build();

            userBranchRepository.save(relation);
        }
    }

    private void updateUsers(
            Branch branch,
            List<UUID> userIds
    ) {

        if (userIds == null) {
            return;
        }

        userBranchRepository.deleteByBranchId(
                tenantId(),
                branch.getId()
        );

        assignUsers(branch, userIds);
    }

    private void updateDepartments(
            Branch branch,
            List<UUID> departmentIds
    ) {

        if (departmentIds == null) {
            return;
        }

        Set<Department> departments =
                departmentIds.stream()
                        .map(
                                id ->
                                        departmentRepository
                                                .findByTenantIdAndIdAndDeletedFalse(
                                                        tenantId(),
                                                        id
                                                )
                                                .orElseThrow(
                                                        () ->
                                                                new EntityNotFoundException(
                                                                        "Department not found: " + id
                                                                )
                                                )
                        )
                        .collect(Collectors.toSet());

        branch.setDepartments(departments);
    }

    private void updateField(
            Branch branch,
            String field,
            String oldValue,
            String newValue,
            java.util.function.Consumer<String> updater,
            Authentication authentication
    ) {

        if (!Objects.equals(oldValue, newValue)) {

            updater.accept(newValue);

            recordAudit(
                    branch,
                    "UPDATE",
                    field,
                    oldValue,
                    newValue,
                    authentication,
                    "Field updated"
            );
        }
    }

    private void validateAttendanceSettings(
            BranchAttendanceSettingsUpdateDTO request
    ) {

        if (
                request.getRollcallStartTime() != null
                        && request.getLogoutTime() != null
                        && !request.getLogoutTime().isAfter(
                        request.getRollcallStartTime()
                )
        ) {
            throw new IllegalArgumentException(
                    "logoutTime must be after rollcallStartTime"
            );
        }
    }

    private void validateSecuritySettings(
            BranchSecuritySettingsUpdateDTO request
    ) {

        boolean geofenceEnabled =
                Boolean.TRUE.equals(
                        request.getEnforceGeofence()
                );

        if (!geofenceEnabled) {
            return;
        }

        if (request.getLatitude() == null) {
            throw new IllegalArgumentException(
                    "latitude is required when geofence enforcement is enabled"
            );
        }

        if (request.getLongitude() == null) {
            throw new IllegalArgumentException(
                    "longitude is required when geofence enforcement is enabled"
            );
        }

        if (request.getRadiusMeters() == null) {
            throw new IllegalArgumentException(
                    "radiusMeters is required when geofence enforcement is enabled"
            );
        }
    }

    private <T> void updateAttendanceField(
            Branch branch,
            String field,
            T oldValue,
            T newValue,
            java.util.function.Consumer<T> updater,
            Authentication authentication
    ) {

        if (newValue == null) {
            return;
        }

        if (!Objects.equals(oldValue, newValue)) {

            updater.accept(newValue);

            recordAudit(
                    branch,
                    "ATTENDANCE_SETTINGS_UPDATE",
                    field,
                    Objects.toString(oldValue, null),
                    Objects.toString(newValue, null),
                    authentication,
                    "Attendance settings updated"
            );
        }
    }

    private <T> void updateSecurityField(
            Branch branch,
            String field,
            T oldValue,
            T newValue,
            java.util.function.Consumer<T> updater,
            Authentication authentication
    ) {

        if (newValue == null) {
            return;
        }

        if (!Objects.equals(oldValue, newValue)) {

            updater.accept(newValue);

            recordAudit(
                    branch,
                    "SECURITY_SETTINGS_UPDATE",
                    field,
                    Objects.toString(oldValue, null),
                    Objects.toString(newValue, null),
                    authentication,
                    "Security settings updated"
            );
        }
    }

    private void invalidateBranchCacheAfterCommit() {

        UUID tenantId = tenantId();

        TransactionSynchronizationManager
                .registerSynchronization(
                        new TransactionSynchronization() {
                            @Override
                            public void afterCommit() {
                                tenantMetadataCache
                                        .invalidateBranchCache(
                                                tenantId
                                        );
                            }
                        }
                );
    }

    private void recordAudit(
            Branch branch,
            String action,
            String field,
            String oldValue,
            String newValue,
            Authentication authentication,
            String reason
    ) {

        User user =
                privilegesChecker.getAuthenticatedUser(
                        authentication
                );

        BranchAudit audit =
                BranchAudit.builder()
                        .branchId(branch.getId())
                        .branchName(branch.getName())
                        .tenantId(tenantId())
                        .action(action)
                        .fieldChanged(field)
                        .oldValue(oldValue)
                        .newValue(newValue)
                        .reason(reason)
                        .performedById(user.getId())
                        .performedByUsername(user.getUsername())
                        .build();

        branchAuditRepository.save(audit);
    }
}