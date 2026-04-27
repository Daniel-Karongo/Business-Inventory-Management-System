import { CommonModule } from "@angular/common";
import { Component, Inject } from "@angular/core";
import { FormsModule } from "@angular/forms";
import { MatButtonModule } from "@angular/material/button";
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from "@angular/material/dialog";
import { MatInputModule } from "@angular/material/input";

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

<div class="rename-dialog">

  <header class="dialog-header">
    <h2 mat-dialog-title class="dialog-title">
      {{ title }}
    </h2>

    <p class="dialog-subtitle">
      {{ subtitle }}
    </p>
  </header>

  <mat-dialog-content class="dialog-content">

    <mat-form-field
      appearance="fill"
      class="full-width">

      <mat-label>{{ label }}</mat-label>

      <input
        matInput
        maxlength="64"
        autocomplete="off"
        [(ngModel)]="name"
        (keydown.enter)="save()"
        cdkFocusInitial />

      <mat-hint align="end">
        {{ name.length || 0 }}/64
      </mat-hint>

    </mat-form-field>

  </mat-dialog-content>

  <mat-dialog-actions
    align="end"
    class="dialog-actions">

    <button
      mat-stroked-button
      type="button"
      (click)="close()">
      Cancel
    </button>

    <button
      mat-flat-button
      color="primary"
      type="button"
      [disabled]="!name.trim()"
      (click)="save()">

      Save Changes

    </button>

  </mat-dialog-actions>

</div>

`,
  styles: [`

:host {
  display: block;
}

.rename-dialog {
  width: min(100%, 520px);
  padding: clamp(20px, 3vw, 28px);
  background: var(--surface);
}

.dialog-header {
  margin-bottom: 20px;
}

.dialog-title {
  margin: 0;
  font-size: clamp(1.15rem, 2vw, 1.35rem);
  font-weight: 600;
  line-height: 1.25;
}

.dialog-subtitle {
  margin: 8px 0 0;
  font-size: .92rem;
  line-height: 1.45;
  color: var(--text-secondary);
}

.dialog-content {
  padding: 0 !important;
  overflow: visible;
}

.full-width {
  width: 100%;
}

.dialog-actions {
  margin-top: 24px;
  padding: 0 !important;
  gap: 12px;
  display: flex;
  flex-wrap: wrap;
}

.dialog-actions button {
  min-width: 120px;
}

@media (max-width: 480px) {

  .rename-dialog {
    padding: 18px;
  }

  .dialog-actions {
    flex-direction: column-reverse;
    align-items: stretch;
  }

  .dialog-actions button {
    width: 100%;
  }

}

`]
})
export class RenameDeviceDialogComponent {

  name = '';
  title = '';
  subtitle = '';
  label = '';

  constructor(
    private ref: MatDialogRef<RenameDeviceDialogComponent>,
    @Inject(MAT_DIALOG_DATA) data: {
      currentName: string;
      title?: string;
      subtitle?: string;
      label?: string;
    }
  ) {

    this.name = data.currentName;

    if (data.title) {
      this.title = data.title;
    }

    if (data.subtitle) {
      this.subtitle = data.subtitle;
    }

    if (data.label) {
      this.label = data.label;
    }

  }

  close() {
    this.ref.close();
  }

  save() {

    const value = this.name?.trim();

    if (!value) {
      return;
    }

    this.ref.close(value);
  }

}