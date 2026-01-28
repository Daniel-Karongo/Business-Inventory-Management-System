package com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
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
    public void recordInventoryReceipt(UUID refId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("INVENTORY_RECEIPT")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory received")
                        .performedBy("SYSTEM")
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.inventory())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.accountsPayable())
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryConsumption(UUID refId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("INVENTORY_CONSUMPTION")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory sold")
                        .performedBy("SYSTEM")
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.cogs())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.inventory())
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryReturn(UUID refId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("INVENTORY_RETURN")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory returned")
                        .performedBy("SYSTEM")
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.inventory())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.cogs())
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryTransferOut(UUID refId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("INVENTORY_TRANSFER_OUT")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory transferred out (source branch)")
                        .performedBy("SYSTEM")
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.inventory())
                                        .direction(EntryDirection.CREDIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }

    @Override
    public void recordInventoryTransferIn(UUID refId, BigDecimal value, String ref) {

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("INVENTORY_TRANSFER_IN")
                        .sourceId(refId)
                        .reference(ref)
                        .description("Inventory transferred in (destination branch)")
                        .performedBy("SYSTEM")
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.inventory())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(value)
                                        .build()
                        ))
                        .build()
        );
    }
}