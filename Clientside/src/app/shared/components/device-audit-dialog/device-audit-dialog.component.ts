import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  MAT_DIALOG_DATA,
  MatDialogModule
} from '@angular/material/dialog';

import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';

@Component({
  standalone: true,
  selector: 'app-device-audit-dialog',
  imports: [
    CommonModule,
    MatDialogModule,
    MatTableModule,
    MatButtonModule,
    MatSelectModule
  ],
  template: `

<h2 class="text-xl font-semibold">Device Audit History</h2>

<mat-dialog-content>

<table
 mat-table
 [dataSource]="data">

<ng-container matColumnDef="action">
<th mat-header-cell *matHeaderCellDef>
Action
</th>
<td mat-cell *matCellDef="let r">
{{r.action}}
</td>
</ng-container>

<ng-container matColumnDef="reason">
<th mat-header-cell *matHeaderCellDef>
Reason
</th>
<td mat-cell *matCellDef="let r">
{{r.reason || '—'}}
</td>
</ng-container>

<ng-container matColumnDef="actedAt">
<th mat-header-cell *matHeaderCellDef>
When
</th>
<td mat-cell *matCellDef="let r">
{{r.actedAt | date:'medium'}}
</td>
</ng-container>

<tr mat-header-row
 *matHeaderRowDef="cols"></tr>

<tr mat-row
 *matRowDef="
 let row;
 columns: cols">
</tr>

</table>

<div *ngIf="!data?.length">
No audit history
</div>

</mat-dialog-content>

<mat-dialog-actions align="end">
<button
mat-flat-button
mat-dialog-close>
Close
</button>
</mat-dialog-actions>
`
})
export class DeviceAuditDialogComponent {

  cols = [
    'action',
    'reason',
    'actedAt'
  ];

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: any[]
  ) { }

}