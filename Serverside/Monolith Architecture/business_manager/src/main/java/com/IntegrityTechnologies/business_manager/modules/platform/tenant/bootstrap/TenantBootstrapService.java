package com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantBootstrapService {

    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;
    private final UserRepository userRepository;

    private final BranchAuditRepository branchAuditRepository;
    private final DepartmentAuditRepository departmentAuditRepository;
    private final UserAuditRepository userAuditRepository;

    private final BranchAccountingSettingsRepository accountingSettingsRepository;
    private final BranchChartOfAccountsService chartService;

    private final PasswordEncoder passwordEncoder;

    @Transactional
    public boolean bootstrapTenant(UUID tenantId, String adminUsername, String adminPassword) {

        /* =====================================
           1️⃣ ENSURE MAIN BRANCH
        ===================================== */

        Branch branch = branchRepository
                .findByTenantIdAndBranchCodeIgnoreCase(tenantId, "MAIN")
                .orElseGet(() -> {

                    Branch b = Branch.builder()
                            .tenantId(tenantId)
                            .branchCode("MAIN")
                            .name("Main Branch")
                            .location("Default Location")
                            .phone("+254700000000")
                            .email("main@tenant.com")
                            .deleted(false)
                            .build();

                    branchRepository.save(b);

                    branchAuditRepository.save(
                            BranchAudit.builder()
                                    .branchId(b.getId())
                                    .branchName(b.getName())
                                    .action("CREATE")
                                    .reason("Tenant bootstrap")
                                    .performedByUsername("system")
                                    .build()
                    );

                    return b;
                });

        /* =====================================
           2️⃣ ENSURE ACCOUNTING SETTINGS
        ===================================== */

        if (!accountingSettingsRepository.existsByBranchId(branch.getId())) {

            chartService.seedForBranch(branch.getId());

            accountingSettingsRepository.save(
                    BranchAccountingSettings.builder()
                            .branchId(branch.getId())
                            .revenueRecognitionMode(RevenueRecognitionMode.DELIVERY)
                            .build()
            );
        }

        /* =====================================
           3️⃣ ENSURE GENERAL DEPARTMENT
        ===================================== */

        Department department = departmentRepository
                .findByNameIgnoreCaseAndBranch_Id("GENERAL", branch.getId())
                .orElseGet(() -> {

                    Department d = Department.builder()
                            .branch(branch)
                            .name("GENERAL")
                            .description("Default general department")
                            .rollcallStartTime(LocalTime.of(9, 0))
                            .gracePeriodMinutes(15)
                            .deleted(false)
                            .build();

                    departmentRepository.save(d);

                    departmentAuditRepository.save(
                            DepartmentAudit.builder()
                                    .departmentId(d.getId())
                                    .departmentName(d.getName())
                                    .action("CREATE")
                                    .reason("Tenant bootstrap")
                                    .performedByUsername("system")
                                    .build()
                    );

                    return d;
                });

        /* =====================================
        4️⃣ ENSURE ADMIN USER
        ===================================== */

        boolean adminCreated = false;

        User admin = userRepository
                .findByUsernameAndTenantId(adminUsername, tenantId)
                .orElse(null);

        if (admin == null) {

            adminCreated = true;

            admin = User.builder()
                    .tenantId(tenantId)
                    .username(adminUsername)
                    .password(passwordEncoder.encode(adminPassword))
                    .role(Role.SUPERUSER)
                    .uploadFolder(UUID.randomUUID().toString())
                    .mustChangePassword(true)
                    .deleted(false)
                    .build();

            userRepository.save(admin);

            userAuditRepository.save(
                    UserAudit.builder()
                            .userId(admin.getId())
                            .username(admin.getUsername())
                            .action("CREATE")
                            .reason("Tenant bootstrap")
                            .performedByUsername("system")
                            .build()
            );
        }

        /* =====================================
           5️⃣ ENSURE USER ↔ BRANCH
        ===================================== */
        boolean adminChanged = adminCreated;

        boolean branchAssigned = admin.getBranches()
                .stream()
                .anyMatch(b -> b.getBranch().getId().equals(branch.getId()));

        if (!branchAssigned) {

            UserBranch userBranch = UserBranch.builder()
                    .user(admin)
                    .branch(branch)
                    .primaryBranch(true)
                    .build();

            admin.getBranches().add(userBranch);
            adminChanged = true;
        }

        /* =====================================
           6️⃣ ENSURE USER ↔ DEPARTMENT
        ===================================== */

        boolean deptAssigned = admin.getDepartments()
                .stream()
                .anyMatch(d -> d.getDepartment().getId().equals(department.getId()));

        if (!deptAssigned) {

            UserDepartment userDepartment = UserDepartment.builder()
                    .user(admin)
                    .department(department)
                    .role(DepartmentMembershipRole.HEAD)
                    .primaryDepartment(true)
                    .build();

            admin.getDepartments().add(userDepartment);
            adminChanged = true;
        }

        if (adminChanged) {
            userRepository.save(admin);
        }
        return adminCreated;
    }
}