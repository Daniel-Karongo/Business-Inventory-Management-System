import { CommonModule } from "@angular/common";
import { Component } from "@angular/core";
import { FormsModule } from "@angular/forms";
import { MatButtonModule } from "@angular/material/button";
import { MatCheckboxModule } from "@angular/material/checkbox";
import { MatDialogModule, MatDialogRef } from "@angular/material/dialog";

@Component({
  standalone: true,
  template: `
    <h2 mat-dialog-title>Restore Options</h2>

    <mat-dialog-content>

      <mat-checkbox
        [(ngModel)]="restoreTransactions"
        (change)="syncCheckboxes($event.checked)"
      >
        Restore stock transactions
      </mat-checkbox>

      <mat-checkbox
        [(ngModel)]="restoreInventory"
        (change)="syncCheckboxes($event.checked)"
      >
        Recalculate inventory from transactions
      </mat-checkbox>

    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-button (click)="close()">Cancel</button>
      <button mat-flat-button color="primary" (click)="confirm()">
        Confirm
      </button>
    </mat-dialog-actions>
  `,
  imports: [
    CommonModule,
    MatCheckboxModule,
    MatButtonModule,
    FormsModule,
    MatDialogModule
  ]
})
export class ProductRestoreOptionsDialog {

  restoreTransactions = true;
  restoreInventory = true;

  constructor(
    private ref: MatDialogRef<ProductRestoreOptionsDialog>
  ) {}

  syncCheckboxes(checked: boolean) {
    this.restoreTransactions = checked;
    this.restoreInventory = checked;
  }

  confirm() {
    this.ref.close({
      restoreStockTransactions: this.restoreTransactions,
      restoreInventory: this.restoreInventory
    });
  }

  close() {
    this.ref.close(null);
  }
}