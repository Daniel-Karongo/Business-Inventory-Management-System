package com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchChartOfAccountsService {

    private final AccountRepository repo;

    @Transactional
    public void seedForBranch(UUID tenantId, UUID branchId) {

        seed(tenantId, branchId,"1000","Cash",AccountType.ASSET,AccountRole.CASH);
        seed(tenantId, branchId,"1100","Bank",AccountType.ASSET,AccountRole.BANK);
        seed(tenantId, branchId,"1150","M-Pesa Clearing Account",AccountType.ASSET,AccountRole.MPESA);
        seed(tenantId, branchId,"1200","Inventory",AccountType.ASSET,AccountRole.INVENTORY);
        seed(tenantId, branchId,"1300","Input VAT",AccountType.ASSET,AccountRole.VAT_INPUT);
        seed(tenantId, branchId,"1500","Accounts Receivable",AccountType.ASSET,AccountRole.ACCOUNTS_RECEIVABLE);

        seed(tenantId, branchId,"2000","Accounts Payable",AccountType.LIABILITY,AccountRole.ACCOUNTS_PAYABLE);
        seed(tenantId, branchId,"2100","Output VAT",AccountType.LIABILITY,AccountRole.VAT_OUTPUT);
        seed(tenantId, branchId,"2200","VAT Payable",AccountType.LIABILITY,AccountRole.VAT_PAYABLE);
        seed(tenantId, branchId,"2300","Branch Clearing",AccountType.LIABILITY,AccountRole.BRANCH_CLEARING);
        seed(tenantId, branchId,"2400","Corporate Tax Payable",AccountType.LIABILITY,AccountRole.CORPORATE_TAX_PAYABLE);

        seed(tenantId, branchId,"3000","Owner Equity",AccountType.EQUITY,AccountRole.EQUITY);

        seed(tenantId, branchId,"4000","Sales Revenue",AccountType.INCOME,AccountRole.REVENUE);

        seed(tenantId, branchId,"5000","Cost Of Goods Sold",AccountType.EXPENSE,AccountRole.COGS);
        seed(tenantId, branchId,"5100","Corporate Tax Expense",AccountType.EXPENSE,AccountRole.CORPORATE_TAX_EXPENSE);
    }

    private void seed(
            UUID tenantId,
            UUID branchId,
            String code,
            String name,
            AccountType type,
            AccountRole role
    ) {

        repo.findByTenantIdAndBranchIdAndCode(
                tenantId,
                branchId,
                code
        ).orElseGet(() ->
                repo.save(new Account(
                        tenantId,
                        branchId,
                        code,
                        name,
                        type,
                        role
                ))
        );
    }
}