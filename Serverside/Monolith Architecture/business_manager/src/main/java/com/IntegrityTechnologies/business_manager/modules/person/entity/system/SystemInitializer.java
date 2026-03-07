package com.IntegrityTechnologies.business_manager.modules.person.entity.system;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalTime;
import java.util.HashSet;
import java.util.UUID;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class SystemInitializer {

    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;
    private final BranchAccountingSettingsRepository accountingSettingsRepository;
    private final BranchChartOfAccountsService chartService;
    private final TenantRepository tenantRepository;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final BranchAuditRepository branchAuditRepository;
    private final DepartmentAuditRepository departmentAuditRepository;
    private final UserAuditRepository userAuditRepository;

    @Bean
    @Transactional
    public ApplicationRunner initializeDefaults() {

        return args -> {

            Tenant tenant = tenantRepository.findByCode("default")
                    .orElseThrow();

            Branch mainBranch = branchRepository.findByBranchCode("MAIN")
                    .orElseGet(() -> createMainBranch(tenant));

            ensureAccountingSettings(mainBranch);

            ensureGeneralDepartment(mainBranch);

            ensureDefaultAdmin(tenant, mainBranch);

            log.info("🎉 System initialization complete.");
        };
    }

    private Branch createMainBranch(Tenant tenant) {

        log.warn("⚠️ No branches found. Creating default MAIN branch.");

        Branch branch = Branch.builder()
                .tenantId(tenant.getId())
                .branchCode("MAIN")
                .name("Main Branch")
                .location("Default Location")
                .phone("+254700000000")
                .email("main@default.com")
                .deleted(false)
                .build();

        branchRepository.save(branch);

        branchAuditRepository.save(
                BranchAudit.builder()
                        .branchId(branch.getId())
                        .branchName(branch.getName())
                        .action("CREATE")
                        .reason("System initialization")
                        .performedByUsername("system")
                        .build()
        );

        chartService.seedForBranch(branch.getId());

        return branch;
    }

    private void ensureAccountingSettings(Branch branch) {

        if (!accountingSettingsRepository.existsByBranchId(branch.getId())) {

            accountingSettingsRepository.save(
                    BranchAccountingSettings.builder()
                            .branchId(branch.getId())
                            .revenueRecognitionMode(
                                    com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode.DELIVERY
                            )
                            .build()
            );
        }
    }

    private void ensureGeneralDepartment(Branch branch) {

        Department dept = departmentRepository.findByNameIgnoreCase("GENERAL")
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
                                    .reason("System initialization")
                                    .performedByUsername("system")
                                    .build()
                    );

                    return d;
                });
    }

    private void ensureDefaultAdmin(Tenant tenant, Branch mainBranch) {

        boolean exists = userRepository.existsByUsername("admin");

        if (exists) return;

        User admin = User.builder()
                .tenantId(tenant.getId())
                .username("admin")
                .password(passwordEncoder.encode("admin123"))
                .role(Role.SUPERUSER)
                .uploadFolder(UUID.randomUUID().toString())
                .deleted(false)
                .build();

        userRepository.save(admin);

        userAuditRepository.save(
                UserAudit.builder()
                        .userId(admin.getId())
                        .username(admin.getUsername())
                        .action("CREATE")
                        .reason("System initialization")
                        .performedByUsername("system")
                        .build()
        );
    }
}