import { CommonModule, CurrencyPipe } from "@angular/common";
import { Component, Inject, OnInit } from "@angular/core";
import { FormArray, FormBuilder, FormControl, FormGroup, ReactiveFormsModule } from "@angular/forms";
import { MatButtonModule } from "@angular/material/button";
import { MatCheckboxModule } from "@angular/material/checkbox";
import { MAT_DIALOG_DATA, MatDialogRef } from "@angular/material/dialog";
import { MatInputModule } from "@angular/material/input";
import { MatSnackBar } from "@angular/material/snack-bar";
import { MatTableModule } from "@angular/material/table";
import { InventoryService } from "../../../inventory/services/inventory.service";

type BatchGroup = FormGroup<{
    batchId: FormControl<string>;
    receivedAt: FormControl<string>;
    available: FormControl<number>;
    unitCost: FormControl<number>;
    quantity: FormControl<number>;
    isSuggested: FormControl<boolean>;
}>;

@Component({
    standalone: true,
    selector: 'app-batch-allocation-dialog',
    templateUrl: './batch-allocation-dialog.component.html',
    styleUrls: ['./batch-allocation-dialog.component.scss'],
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatInputModule,
        MatTableModule,
        CurrencyPipe,
        MatCheckboxModule
    ]
})
export class BatchAllocationDialogComponent implements OnInit {

    form!: FormGroup<{
        allocations: FormArray<BatchGroup>;
    }>;

    displayedColumns = ['receivedAt', 'available', 'unitCost', 'allocate'];

    averageCost = 0;
    baselineFifoCost = 0;
    useFifo = true;

    constructor(
        private fb: FormBuilder,
        private inventoryService: InventoryService,
        private snackBar: MatSnackBar,
        public dialogRef: MatDialogRef<BatchAllocationDialogComponent>,
        @Inject(MAT_DIALOG_DATA)
        public data: {
            variantId: string;
            branchId: string;
            quantity: number;
        }
    ) { }

    ngOnInit() {
        this.form = new FormGroup({
            allocations: new FormArray<BatchGroup>([])
        });

        this.loadSuggested();
    }

    get tableRows(): BatchGroup[] {
        return [...this.allocations.controls];
    }
    
    private createBatchGroup(data: {
        batchId: string;
        receivedAt: string;
        available: number;
        unitCost: number;
        quantity: number;
        isSuggested: boolean;
    }) {
        return new FormGroup({
            batchId: new FormControl<string>(data.batchId, { nonNullable: true }),
            receivedAt: new FormControl<string>(data.receivedAt, { nonNullable: true }),
            available: new FormControl<number>(data.available, { nonNullable: true }),
            unitCost: new FormControl<number>(data.unitCost, { nonNullable: true }),
            quantity: new FormControl<number>(data.quantity, { nonNullable: true }),
            isSuggested: new FormControl<boolean>(data.isSuggested, { nonNullable: true })
        });
    }

    onFifoToggle(checked: boolean) {
        this.useFifo = checked;

        if (checked) {
            this.applyFifo();
        }

        this.allocations.updateValueAndValidity();
    }

    costDeviation(): number {
        return this.totalCost() - this.baselineFifoCost;
    }

    get allocations() {
        return this.form.controls.allocations;
    }

    quantityControl(i: number): FormControl<number> {
        return this.allocations.at(i).get('quantity') as FormControl<number>;
    }

    applyFifo() {
        let remaining = this.data.quantity;

        this.allocations.controls.forEach(control => {

            const available = control.controls.available.value;
            const allocate = Math.min(available, remaining);

            control.controls.quantity.setValue(allocate, { emitEvent: false });
            remaining -= allocate;
        });
    }

    isAllocationValid(): boolean {
        return this.totalAllocated() === this.data.quantity;
    }

    loadSuggested() {
        this.inventoryService
            .getAllBatches(
                this.data.variantId,
                this.data.branchId
            )
            .subscribe((batches: any[]) => {

                this.allocations.clear();

                let remaining = this.data.quantity ?? 0;

                batches.forEach((b: any) => {

                    const suggested = Math.min(
                        b.quantityRemaining,
                        remaining
                    );

                    remaining -= suggested;

                    const group = this.createBatchGroup({
                        batchId: b.batchId,
                        receivedAt: b.receivedAt,
                        available: Number(b.quantityRemaining),
                        unitCost: Number(b.unitSellingPrice ?? b.unitCost),
                        quantity: suggested,
                        isSuggested: suggested > 0
                    });

                    this.allocations.push(group);

                    group.get('quantity')?.valueChanges.subscribe(val => {

                        if (this.useFifo) return;

                        let qty = Number(val || 0);
                        const max = group.controls.available.value;

                        if (qty > max) qty = max;
                        if (qty < 0) qty = 0;

                        group.get('quantity')?.setValue(qty, { emitEvent: false });

                        const total = this.totalAllocated();

                        if (total > this.data.quantity) {
                            const excess = total - this.data.quantity;
                            group.get('quantity')?.setValue(qty - excess, { emitEvent: false });
                        }
                    });
                });

                this.baselineFifoCost = this.totalCost();
                this.useFifo = true;
            });
    }

    totalAllocated(): number {
        return this.allocations.controls
            .map(c => Number(c.value.quantity || 0))
            .reduce((a, b) => a + b, 0);
    }

    totalCost(): number {
        return this.allocations.controls
            .map(c => {
                const qty = Number(c.value.quantity || 0);
                const cost = Number(c.value.unitCost || 0);
                return qty * cost;
            })
            .reduce((a, b) => a + b, 0);
    }

    save() {

        if (!this.isAllocationValid()) {
            this.snackBar.open(
                `Allocated quantity must equal ${this.data.quantity}`,
                'Close',
                { duration: 3000 }
            );
            return;
        }

        const result = this.allocations.controls
            .filter(c => c.controls.quantity.value > 0)
            .map(c => ({
                batchId: c.controls.batchId.value,
                quantity: c.controls.quantity.value,
                unitCost: c.controls.unitCost.value
            }));

        if (!result.length) {
            this.snackBar.open(
                'No batch selected.',
                'Close',
                { duration: 3000 }
            );
            return;
        }

        this.dialogRef.close(result);
    }

    cancel() {
        this.dialogRef.close();
    }
}