package com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
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

        seed(tenantId, branchId, "1000", "Cash", AccountType.ASSET, "CASH");
        seed(tenantId, branchId, "1100", "Bank", AccountType.ASSET, "BANK");
        seed(tenantId, branchId, "1150", "M-Pesa", AccountType.ASSET, "MPESA");
        seed(tenantId, branchId, "1200", "Inventory", AccountType.ASSET, "INVENTORY");

        seed(tenantId, branchId, "1300", "Input VAT", AccountType.ASSET, "VAT_INPUT");
        seed(tenantId, branchId, "1350", "VAT Receivable", AccountType.ASSET, "VAT_RECEIVABLE");
        seed(tenantId, branchId, "1360", "VAT Credit Carry Forward", AccountType.ASSET, "VAT_CARRY_FORWARD");

        seed(tenantId, branchId, "1500", "Accounts Receivable", AccountType.ASSET, "ACCOUNTS_RECEIVABLE");

        seed(tenantId, branchId, "2000", "Accounts Payable", AccountType.LIABILITY, "ACCOUNTS_PAYABLE");
        seed(tenantId, branchId, "2100", "Output VAT", AccountType.LIABILITY, "VAT_OUTPUT");
        seed(tenantId, branchId, "2200", "VAT Payable", AccountType.LIABILITY, "VAT_PAYABLE");

        seed(tenantId, branchId, "2050", "Goods Received Not Invoiced", AccountType.LIABILITY, "GOODS_RECEIVED_NOT_INVOICED" );

        seed(tenantId, branchId, "2250", "Operational Expense Payable", AccountType.LIABILITY, "OPERATIONAL_EXPENSE_PAYABLE" );

        seed(tenantId, branchId, "2300", "Branch Clearing", AccountType.LIABILITY, "BRANCH_CLEARING");
        seed(tenantId, branchId, "2400", "Corporate Tax Payable", AccountType.LIABILITY, "CORPORATE_TAX_PAYABLE");

        seed(tenantId, branchId, "3000", "Owner Equity", AccountType.EQUITY, "EQUITY");

        seed(tenantId, branchId, "4000", "Sales Revenue", AccountType.INCOME, "REVENUE");

        seed(tenantId, branchId, "5000", "Cost Of Goods Sold", AccountType.EXPENSE, "COGS");
        seed(tenantId, branchId, "5100", "Corporate Tax Expense", AccountType.EXPENSE, "CORPORATE_TAX_EXPENSE");
        seed(tenantId, branchId, "5200", "Operating Expenses", AccountType.EXPENSE, "OPERATING_EXPENSE");
    }

    private void seed(
            UUID tenantId,
            UUID branchId,
            String code,
            String name,
            AccountType type,
            String role
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