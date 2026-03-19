package com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
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

    @Override
    public void recordInventoryReceipt(UUID tenantId, UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(buildEvent(
                tenantId,
                "INVENTORY_RECEIPT",
                refId,
                branchId,
                ref,
                "Inventory received",
                List.of(
                        entry(accounts.get(tenantId, branchId, AccountRole.INVENTORY), EntryDirection.DEBIT, value),
                        entry(accounts.get(tenantId, branchId, AccountRole.ACCOUNTS_PAYABLE), EntryDirection.CREDIT, value)
                )
        ));
    }

    @Override
    public void recordInventoryConsumption(UUID tenantId, UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(buildEvent(
                tenantId,
                "INVENTORY_CONSUMPTION",
                refId,
                branchId,
                ref,
                "Inventory sold",
                List.of(
                        entry(accounts.get(tenantId, branchId, AccountRole.COGS), EntryDirection.DEBIT, value),
                        entry(accounts.get(tenantId, branchId, AccountRole.INVENTORY), EntryDirection.CREDIT, value)
                )
        ));
    }

    @Override
    public void recordInventoryReturn(UUID tenantId, UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(buildEvent(
                tenantId,
                "INVENTORY_RETURN",
                refId,
                branchId,
                ref,
                "Inventory returned",
                List.of(
                        entry(accounts.get(tenantId, branchId, AccountRole.INVENTORY), EntryDirection.DEBIT, value),
                        entry(accounts.get(tenantId, branchId, AccountRole.COGS), EntryDirection.CREDIT, value)
                )
        ));
    }

    @Override
    public void recordInventoryTransferOut(UUID tenantId, UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(buildEvent(
                tenantId,
                "INVENTORY_TRANSFER_OUT",
                refId,
                branchId,
                ref,
                "Inventory transferred out (source branch)",
                List.of(
                        entry(accounts.get(tenantId, branchId, AccountRole.BRANCH_CLEARING), EntryDirection.DEBIT, value),
                        entry(accounts.get(tenantId, branchId, AccountRole.INVENTORY), EntryDirection.CREDIT, value)
                )
        ));
    }

    @Override
    public void recordInventoryTransferIn(UUID tenantId, UUID refId, UUID branchId, BigDecimal value, String ref) {

        accountingFacade.post(buildEvent(
                tenantId,
                "INVENTORY_TRANSFER_IN",
                refId,
                branchId,
                ref,
                "Inventory transferred in (destination branch)",
                List.of(
                        entry(accounts.get(tenantId, branchId, AccountRole.INVENTORY), EntryDirection.DEBIT, value),
                        entry(accounts.get(tenantId, branchId, AccountRole.BRANCH_CLEARING), EntryDirection.CREDIT, value)
                )
        ));
    }

    /* ========================= INTERNAL HELPERS ========================= */

    private AccountingEvent buildEvent(
            UUID tenantId,
            String module,
            UUID sourceId,
            UUID branchId,
            String reference,
            String description,
            List<AccountingEvent.Entry> entries
    ) {
        return AccountingEvent.builder()
                .eventId(UUID.randomUUID())
                .tenantId(tenantId)
                .sourceModule(module)
                .sourceId(sourceId)
                .reference(reference)
                .description(description)
                .performedBy("SYSTEM")
                .branchId(branchId)
                .entries(entries)
                .build();
    }

    private AccountingEvent.Entry entry(UUID accountId, EntryDirection direction, BigDecimal amount) {
        return AccountingEvent.Entry.builder()
                .accountId(accountId)
                .direction(direction)
                .amount(amount)
                .build();
    }
}