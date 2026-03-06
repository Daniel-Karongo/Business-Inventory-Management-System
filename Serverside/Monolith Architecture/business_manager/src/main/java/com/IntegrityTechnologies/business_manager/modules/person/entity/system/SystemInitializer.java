package com.IntegrityTechnologies.business_manager.modules.person.entity.system;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalTime;
import java.util.HashSet;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class SystemInitializer {

    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;
    private final BranchAccountingSettingsRepository accountingSettingsRepository;
    private final BranchChartOfAccountsService chartService;
    private final TenantRepository tenantRepository;

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

            log.info("🎉 System initialization complete.");
        };
    }

    private Branch createMainBranch(Tenant tenant) {

        log.warn("⚠️ No branches found. Creating default MAIN branch.");

        Branch branch = Branch.builder()
                .tenant(tenant)
                .branchCode("MAIN")
                .name("Main Branch")
                .location("Default Location")
                .phone("+254700000000")
                .email("main@default.com")
                .build();

        branchRepository.save(branch);

        chartService.seedForBranch(branch.getId());

        log.info("✅ Default MAIN branch created with chart of accounts.");

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

                    log.warn("⚠️ No departments found. Creating GENERAL department.");

                    Department d = Department.builder()
                            .branch(branch)   // FIX: assign branch before save
                            .name("GENERAL")
                            .description("Default general department")
                            .rollcallStartTime(LocalTime.of(9, 0))
                            .gracePeriodMinutes(15)
                            .build();

                    departmentRepository.save(d);

                    log.info("✅ Default GENERAL department created.");

                    return d;
                });

        if (dept.getBranch() == null) {

            dept.setBranch(branch);

            departmentRepository.save(dept);

            log.info("✅ GENERAL department linked to MAIN branch.");
        }
    }
}