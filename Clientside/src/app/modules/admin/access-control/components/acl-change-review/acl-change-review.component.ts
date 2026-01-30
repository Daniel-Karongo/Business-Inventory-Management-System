import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatDialogModule,
    MatFormFieldModule,
    MatInputModule
  ],
  templateUrl: './acl-change-review.component.html',
  styleUrls: ['./acl-change-review.component.scss']
})
export class AclChangeReviewComponent {

  reason = '';

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private ref: MatDialogRef<AclChangeReviewComponent>
  ) { }

  confirm() {
    if (!this.reason.trim()) return;
    this.ref.close(this.reason);
  }

  cancel() {
    this.ref.close(null);
  }
}