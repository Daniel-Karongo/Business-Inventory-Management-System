import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
    MAT_DIALOG_DATA,
    MatDialogRef,
    MatDialogModule
} from '@angular/material/dialog';

import { FormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';

@Component({
    standalone: true,
    imports: [
        CommonModule,
        FormsModule,
        MatDialogModule,
        MatInputModule,
        MatButtonModule
    ],
    template: `

<h2 class="text-xl font-semibold">Rename Device</h2>

<mat-dialog-content>

<mat-form-field appearance="fill">
<input
matInput
[(ngModel)]="name">
</mat-form-field>

</mat-dialog-content>

<mat-dialog-actions align="end">

<button
mat-stroked-button
(click)="close()">
Cancel
</button>

<button
mat-flat-button
(click)="save()">
Save
</button>

</mat-dialog-actions>

`
})
export class RenameDeviceDialogComponent {

    name = '';

    constructor(
        private ref: MatDialogRef<RenameDeviceDialogComponent>,
        @Inject(MAT_DIALOG_DATA) data: any
    ) {
        this.name = data.currentName;
    }

    close() {
        this.ref.close();
    }

    save() {
        this.ref.close(this.name);
    }

}
