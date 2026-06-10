import {
    BulkImportConfig
} from '../../../../../../../shared/bulk-import/models/bulk-import-config.model';
import {
    BulkRequest
} from '../../../../../../../shared/models/bulk-import.model';
import {
    BulkStockOnboardingRequest,
    StockOnboardingRequest
} from '../../../models/stock-onboarding.model';

export interface StockOnboardingBulkRow {
    productName: string;
    classification?: string;
    categoryName?: string;
    branchCode: string;
    packagingName: string;
    packagingUnits: number;
    supplierName: string;
    unitsSupplied: number;
    unitCost: number;
    vatInclusive: boolean;
    vatRate: number;
    sellingPrice: number;
    reference?: string;
    note?: string;
}

export const
    STOCK_ONBOARDING_BULK_IMPORT_CONFIG:
        BulkImportConfig<
            StockOnboardingBulkRow,
            StockOnboardingRequest,
            any
        > = {

        title:
            'Bulk Product + Inventory Onboarding',

        confirmLabel:
            'Confirm Onboarding',

        csvFileName:
            'stock-onboarding-template.csv',

        excelFileName:
            'stock-onboarding-template.xlsx',

        supportsArchiveImport:
            false,

        headers: [
            'productName',
            'classification',
            'categoryName',
            'branchCode',
            'packagingName',
            'packagingUnits',
            'supplierName',
            'unitsSupplied',
            'unitCost',
            'vatInclusive',
            'vatRate',
            'sellingPrice',
            'reference',
            'note'
        ],

        fields: [
            {
                name: 'productName',
                required: true
            },
            {
                name: 'classification',
                defaultValue: 'STANDARD'
            },
            {
                name: 'categoryName'
            },
            {
                name: 'branchCode',
                required: true
            },
            {
                name: 'packagingName',
                required: true,
                defaultValue: 'Piece'
            },
            {
                name: 'packagingUnits',
                required: true,
                defaultValue: 1
            },
            {
                name: 'supplierName',
                required: true
            },
            {
                name: 'unitsSupplied',
                required: true,
                defaultValue: 1
            },
            {
                name: 'unitCost',
                required: true
            },
            {
                name: 'vatInclusive',
                required: true,
                defaultValue: true
            },
            {
                name: 'vatRate',
                required: true,
                defaultValue: 16
            },
            {
                name: 'sellingPrice',
                required: true
            },
            {
                name: 'reference'
            },
            {
                name: 'note'
            }
        ],

        previewColumns: [

            {
                key: 'productName',
                label: 'Product'
            },

            {
                key: 'classification',
                label: 'Variant'
            },

            {
                key: 'totalUnits',
                label: 'Units'
            },

            {
                key: 'grossCost',
                label: 'Inventory Value'
            },

            {
                key: 'vatAmount',
                label: 'VAT'
            },

            {
                key: 'packagingCount',
                label: 'Packagings'
            },

            {
                key: 'pricingCount',
                label: 'Prices'
            },

            {
                key: 'suppliersCreated',
                label: 'New Suppliers'
            },

            {
                key: 'existingProduct',
                label: 'Existing Product'
            },

            {
                key: 'existingVariant',
                label: 'Existing Variant'
            }

        ],

        excelSheetName:
            'Stock Onboarding',

        excelColumns: [
            {
                header: 'productName',
                key: 'productName',
                width: 30
            },
            {
                header: 'classification',
                key: 'classification',
                width: 20
            },
            {
                header: 'categoryName',
                key: 'categoryName',
                width: 24
            },
            {
                header: 'branchCode',
                key: 'branchCode',
                width: 18
            },
            {
                header: 'packagingName',
                key: 'packagingName',
                width: 22
            },
            {
                header: 'packagingUnits',
                key: 'packagingUnits',
                width: 18
            },
            {
                header: 'supplierName',
                key: 'supplierName',
                width: 26
            },
            {
                header: 'unitsSupplied',
                key: 'unitsSupplied',
                width: 18
            },
            {
                header: 'unitCost',
                key: 'unitCost',
                width: 16
            },
            {
                header: 'vatInclusive',
                key: 'vatInclusive',
                width: 16
            },
            {
                header: 'vatRate',
                key: 'vatRate',
                width: 14
            },
            {
                header: 'sellingPrice',
                key: 'sellingPrice',
                width: 16
            },
            {
                header: 'reference',
                key: 'reference',
                width: 18
            },
            {
                header: 'note',
                key: 'note',
                width: 28
            }
        ],

        exampleRow: {
            productName:
                'Milk 1L',
            classification:
                'STANDARD',
            categoryName:
                'Dairy',
            branchCode:
                'MAIN',
            packagingName:
                'Carton',
            packagingUnits:
                1,
            supplierName:
                'Local Supplier',
            unitsSupplied:
                48,
            unitCost:
                2500,
            vatInclusive:
                true,
            vatRate:
                16,
            sellingPrice:
                3200,
            reference:
                'RECEIPT:GRN-001',
            note:
                'Initial stock'
        },

        emptyRow: {
            classification:
                'STANDARD',
            packagingName:
                'Piece',
            packagingUnits:
                1,
            unitsSupplied:
                1,
            vatInclusive:
                true,
            vatRate:
                16
        },

        emptyRowCount: 200,
        defaultOptions: {
            dryRun: true
        },

        mapRowToItem() {
            return {} as StockOnboardingRequest;
        },

        mapPreviewRow(row: any) {
            return {
                ...row,
                existingProduct:
                    row.existingProduct
                        ? 'YES'
                        : 'NO',
                existingVariant:
                    row.existingVariant
                        ? 'YES'
                        : 'NO',
                categoryCreated:
                    row.categoryCreated
                        ? 'YES'
                        : 'NO'
            };
        },

        submit(
            _: BulkRequest<
                StockOnboardingRequest
            >
        ) {
            return null as any;
        }
    };