import {
    Injectable
} from '@angular/core';
import { ReceiptPrintData } from '../../../../../shared/printing/models/receipt-print.model';
import { SaleDTO } from '../../stock/models/sale.model';

@Injectable({
    providedIn: 'root'
})
export class SalePrintMapper {

    map(
        sale: SaleDTO
    ): ReceiptPrintData {

        const subtotal =
            Number(sale.totalAmount ?? 0)
            - Number(sale.totalTax ?? 0);

        return {

            receiptNumber:
                sale.receiptNo,

            createdAt:
                sale.createdAt,

            servedBy:
                sale.createdBy,

            branchName:
                sale.items?.[0]?.branchName
                ?? 'Main Branch',

            customer:
                sale.customer
                    ? {
                        name:
                            sale.customer.name,

                        phone:
                            sale.customer.phoneNumbers?.[0],

                        email:
                            sale.customer.emailAddresses?.[0]
                    }
                    : undefined,

            items:
                sale.items.map(item => ({

                    productName:
                        item.productName,

                    variantName:
                        item.classification,

                    quantity:
                        item.quantity,

                    unitPrice:
                        item.unitPrice,

                    total:
                        item.lineTotal
                })),

            totals: {

                subtotal,

                tax:
                    sale.totalTax ?? 0,

                discount:
                    sale.totalDiscount ?? 0,

                total:
                    sale.totalAmount ?? 0
            },

            payments:
                sale.payments?.map(payment => ({

                    method:
                        payment.method ?? 'UNKNOWN',

                    amount:
                        payment.amount,

                    reference:
                        payment.transactionCode,

                    status:
                        payment.status,

                    timestamp:
                        payment.timestamp
                })),

            footerText:
                'Thank you for your business.'
        };
    }
}