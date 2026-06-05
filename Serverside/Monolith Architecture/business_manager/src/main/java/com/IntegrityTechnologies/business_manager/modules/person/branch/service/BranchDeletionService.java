package com.IntegrityTechnologies.business_manager.modules.person.branch.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLog;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationItem;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationRun;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAudit;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots.DailyAccountBalanceSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoiceLine;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallAudit;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.domain.GoodsReceipt;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSupplier;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImage;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.CustomerPrice;

import com.IntegrityTechnologies.business_manager.modules.person.customer.model.Customer;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerGroup;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.SupplierImage;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.model.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatCreditMovement;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatLedgerProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatRefund;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.projection.CorporateTaxLedgerProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxPayment;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.ProjectionCheckpoint;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerPaymentHistory;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackagingAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPriceAudit;
import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;


import com.IntegrityTechnologies.business_manager.modules.person.branch.deletion.BranchDeletionMode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.io.IOException;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class BranchDeletionService {

    private final BranchEntityBulkOperations bulk;
    private final FileStorageService fileStorageService;

    public void softDelete(
            UUID tenantId,
            UUID branchId,
            BranchDeletionMode mode
    ) {

        switch (mode) {

            case SOFT_CONFIGURATION_ONLY ->
                    softDeleteConfigurationOnly(
                            tenantId,
                            branchId
                    );

            case SOFT_FULL ->
                    softDeleteFull(
                            tenantId,
                            branchId
                    );

            default ->
                    throw new IllegalArgumentException(
                            "Invalid deletion mode"
                    );
        }
    }

    public void restore(
            UUID tenantId,
            UUID branchId,
            BranchDeletionMode mode
    ) {

        switch (mode) {

            case SOFT_CONFIGURATION_ONLY ->
                    restoreConfigurationOnly(
                            tenantId,
                            branchId
                    );

            case SOFT_FULL ->
                    restoreFull(
                            tenantId,
                            branchId
                    );

            default ->
                    throw new IllegalArgumentException(
                            "Invalid restore mode"
                    );
        }
    }

    public void hardDelete(
            UUID tenantId,
            UUID branchId
    ) {
        /*
         * ===============================
         * CHILD TABLES FIRST
         * ===============================
         */

        /*
         * Tax
         */

        bulk.hardDelete(
                VatCreditMovement.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                VatLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                VatPayment.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                VatRefund.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                VatFiling.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                CorporateTaxLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                CorporateTaxPayment.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                CorporateTaxFiling.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                TaxSystemState.class,
                tenantId,
                branchId
        );

        /*
         * Accounting
         */

        bulk.hardDelete(
                ReconciliationItem.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ReconciliationRun.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ReconciliationState.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                GovernanceAuditLog.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                JournalIntegrityAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                LedgerEntry.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                JournalEntry.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                DailyAccountBalanceSnapshot.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                AccountBalance.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Account.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                AccountingPeriod.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BranchAccountingSettings.class,
                tenantId,
                branchId
        );

        /*
         * Sales
         */

        bulk.hardDelete(
                SaleLineBatchSelection.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SaleLineItem.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Payment.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Sale.class,
                tenantId,
                branchId
        );

        /*
         * Procurement
         */

        bulk.hardDelete(
                PurchaseInvoiceLine.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                PurchaseInvoice.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                GoodsReceipt.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SupplierPaymentAllocation.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SupplierPayment.class,
                tenantId,
                branchId
        );

        /*
         * Inventory
         */

        bulk.hardDelete(
                StockTransaction.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                InventorySnapshot.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BatchConsumption.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BatchReservation.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                InventoryBatch.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                InventoryItem.class,
                tenantId,
                branchId
        );

        /*
         * Product hierarchy
         */

        bulk.hardDelete(
                ProductPriceAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductPrice.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                CustomerPrice.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariantPackagingAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariantPackaging.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariantImageAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariantImage.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariantAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductVariant.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductImageAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductImage.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductSupplier.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Product.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                ProductSequence.class,
                tenantId,
                branchId
        );

        /*
         * Category hierarchy
         */

        bulk.hardDelete(
                CategorySupplier.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Category.class,
                tenantId,
                branchId
        );

        /*
         * Supplier hierarchy
         */

        bulk.hardDelete(
                SupplierImageAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SupplierImage.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SupplierAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Supplier.class,
                tenantId,
                branchId
        );

        /*
         * Customer hierarchy
         */

        bulk.hardDelete(
                Customer.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                CustomerGroup.class,
                tenantId,
                branchId
        );

        /*
         * Rollcall
         */

        bulk.hardDelete(
                RollcallAudit.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                Rollcall.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                UserSession.class,
                tenantId,
                branchId
        );

        /*
         * Messaging
         */

        bulk.hardDelete(
                EmailMessage.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                SmsMessage.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                MpesaTransaction.class,
                tenantId,
                branchId
        );

        /*
         * Dashboard
         */

        bulk.hardDelete(
                DashboardDailySnapshot.class,
                tenantId,
                branchId
        );

        /*
         * Branch configuration
         */

        bulk.hardDelete(
                BranchNotificationSettings.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BranchEmailSettings.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BranchSmsSettings.class,
                tenantId,
                branchId
        );

        bulk.hardDelete(
                BranchMpesaSettings.class,
                tenantId,
                branchId
        );

        /*
         * Filesystem
         */

        try {
            fileStorageService.deleteBranchStorage(
                    branchId
            );
        } catch (IOException ex) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();

            throw new RuntimeException(
                    "Failed to delete branch storage",
                    ex
            );
        }
    }

    private void softDeleteConfigurationOnly(
            UUID tenantId,
            UUID branchId
    ) {
        /*
         * Branch configuration.
         */

        bulk.branchDelete(
                BranchNotificationSettings.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BranchEmailSettings.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BranchSmsSettings.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BranchMpesaSettings.class,
                tenantId,
                branchId
        );

        /*
         * Product structure.
         */

        bulk.branchDelete(
                CategorySupplier.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductSupplier.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                CustomerPrice.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductPriceAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductPrice.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariantPackagingAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariantPackaging.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariantImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariantImage.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariantAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductVariant.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductImage.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Product.class,
                tenantId,
                branchId
        );

        /*
         * Supplier structure.
         */

        bulk.branchDelete(
                SupplierImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SupplierImage.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SupplierAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Supplier.class,
                tenantId,
                branchId
        );

        /*
         * Customer structure.
         */

        bulk.branchDelete(
                Customer.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                CustomerGroup.class,
                tenantId,
                branchId
        );

        /*
         * Inventory structure.
         */

        bulk.branchDelete(
                InventorySnapshot.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BatchConsumption.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BatchReservation.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                InventoryBatch.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                InventoryItem.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProductSequence.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Category.class,
                tenantId,
                branchId
        );
    }

    private void softDeleteFull(
            UUID tenantId,
            UUID branchId
    ) {
        /*
         * Configuration.
         */
        softDeleteConfigurationOnly(
                tenantId,
                branchId
        );

        /*
         * Operational.
         */
        bulk.branchDelete(
                SaleLineBatchSelection.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SaleLineItem.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Payment.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Sale.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                PurchaseInvoiceLine.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SupplierPaymentAllocation.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SupplierPayment.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                PurchaseInvoice.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                GoodsReceipt.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                StockTransaction.class,
                tenantId,
                branchId
        );

        /*
         * Customer.
         */

        bulk.branchDelete(
                CustomerPaymentHistory.class,
                tenantId,
                branchId
        );

        /*
         * Attendance.
         */

        bulk.branchDelete(
                RollcallAudit.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Rollcall.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                UserSession.class,
                tenantId,
                branchId
        );

        /*
         * Messaging.
         */

        bulk.branchDelete(
                EmailMessage.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                SmsMessage.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                MpesaTransaction.class,
                tenantId,
                branchId
        );

        /*
         * Dashboard.
         */

        bulk.branchDelete(
                DashboardDailySnapshot.class,
                tenantId,
                branchId
        );

        /*
         * Branch Audit.
         */

        bulk.branchDelete(
                BranchAudit.class,
                tenantId,
                branchId
        );

        /*
         * Accounting Audit.
         */

        bulk.branchDelete(
                GovernanceAuditLog.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                JournalIntegrityAudit.class,
                tenantId,
                branchId
        );

        /*
         * Reconciliation.
         */

        bulk.branchDelete(
                ReconciliationItem.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ReconciliationRun.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ReconciliationState.class,
                tenantId,
                branchId
        );

        /*
         * Accounting.
         */

        bulk.branchDelete(
                LedgerEntry.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                JournalEntry.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                DailyAccountBalanceSnapshot.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                AccountBalance.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                Account.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                ProjectionCheckpoint.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                AccountingPeriod.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                AccountingSystemState.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                BranchAccountingSettings.class,
                tenantId,
                branchId
        );

        /*
         * Tax.
         */

        bulk.branchDelete(
                VatCreditMovement.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                VatPayment.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                VatRefund.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                VatFiling.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                VatLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                CorporateTaxPayment.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                CorporateTaxFiling.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                CorporateTaxLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.branchDelete(
                TaxSystemState.class,
                tenantId,
                branchId
        );
    }

    private void restoreConfigurationOnly(
            UUID tenantId,
            UUID branchId
    ) {
        bulk.branchRestore(
                Category.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductSequence.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                InventoryItem.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                InventoryBatch.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BatchReservation.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BatchConsumption.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                InventorySnapshot.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CustomerGroup.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Customer.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Supplier.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SupplierAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SupplierImage.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SupplierImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Product.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductImage.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariant.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariantAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariantImage.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariantImageAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariantPackaging.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductVariantPackagingAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductPrice.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductPriceAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CustomerPrice.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProductSupplier.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CategorySupplier.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BranchNotificationSettings.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BranchEmailSettings.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BranchSmsSettings.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                BranchMpesaSettings.class,
                tenantId,
                branchId
        );
    }

    private void restoreFull(
            UUID tenantId,
            UUID branchId
    ) {
        restoreConfigurationOnly(
                tenantId,
                branchId
        );

        /*
         * Tax.
         */

        bulk.branchRestore(
                TaxSystemState.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CorporateTaxLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CorporateTaxFiling.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                CorporateTaxPayment.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                VatLedgerProjection.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                VatFiling.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                VatPayment.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                VatRefund.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                VatCreditMovement.class,
                tenantId,
                branchId
        );

        /*
         * Accounting.
         */

        bulk.branchRestore(
                BranchAccountingSettings.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                AccountingSystemState.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                AccountingPeriod.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ProjectionCheckpoint.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Account.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                AccountBalance.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                DailyAccountBalanceSnapshot.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                LedgerEntry.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                JournalEntry.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ReconciliationState.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ReconciliationRun.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                ReconciliationItem.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                JournalIntegrityAudit.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                GovernanceAuditLog.class,
                tenantId,
                branchId
        );

        /*
         * Dashboard.
         */

        bulk.branchRestore(
                DashboardDailySnapshot.class,
                tenantId,
                branchId
        );

        /*
         * Attendance.
         */

        bulk.branchRestore(
                UserSession.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Rollcall.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                RollcallAudit.class,
                tenantId,
                branchId
        );

        /*
         * Messaging.
         */

        bulk.branchRestore(
                EmailMessage.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SmsMessage.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                MpesaTransaction.class,
                tenantId,
                branchId
        );

        /*
         * Branch Audit.
         */

        bulk.branchRestore(
                BranchAudit.class,
                tenantId,
                branchId
        );

        /*
         * Customer.
         */

        bulk.branchRestore(
                CustomerPaymentHistory.class,
                tenantId,
                branchId
        );

        /*
         * Operational.
         */

        bulk.branchRestore(
                GoodsReceipt.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                PurchaseInvoice.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SupplierPayment.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SupplierPaymentAllocation.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                PurchaseInvoiceLine.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                StockTransaction.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Sale.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                Payment.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SaleLineItem.class,
                tenantId,
                branchId
        );

        bulk.branchRestore(
                SaleLineBatchSelection.class,
                tenantId,
                branchId
        );
    }
}