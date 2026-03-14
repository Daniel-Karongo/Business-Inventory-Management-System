package com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting.InventoryAccountingPort;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryAccountingAdapter implements InventoryAccountingPort {

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Override
    public void recordInventoryReceipt(UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("INVENTORY_RECEIPT")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory received")
                        .performedBy("SYSTEM")
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.INVENTORY))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.ACCOUNTS_PAYABLE))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryConsumption(UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("INVENTORY_CONSUMPTION")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory sold")
                        .performedBy("SYSTEM")
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.COGS))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.INVENTORY))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryReturn(UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("INVENTORY_RETURN")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory returned")
                        .performedBy("SYSTEM")
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.INVENTORY))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.COGS))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryTransferOut(UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("INVENTORY_TRANSFER_OUT")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory transferred out (source branch)")
                        .performedBy("SYSTEM")
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.BRANCH_CLEARING))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.INVENTORY))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryTransferIn(UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("INVENTORY_TRANSFER_IN")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory transferred in (destination branch)")
                        .performedBy("SYSTEM")
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.INVENTORY))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(tenantId(), branchId, AccountRole.BRANCH_CLEARING))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }
}