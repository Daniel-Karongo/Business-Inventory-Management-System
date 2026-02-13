import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  selector: 'app-bulk-file-preview-dialog',
  imports: [CommonModule, MatDialogModule],
  template: `
    <div class="preview-wrapper">
      <img *ngIf="isImage" [src]="data.url" />
      <iframe *ngIf="!isImage" [src]="data.url"></iframe>
    </div>
  `,
  styles: [`
    .preview-wrapper {
      max-height: 80vh;
      max-width: 90vw;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    img {
      max-width: 100%;
      max-height: 80vh;
      border-radius: 12px;
    }

    iframe {
      width: 80vw;
      height: 80vh;
      border: none;
    }
  `]
})
export class BulkFilePreviewDialogComponent {

  isImage = true;

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: { url: string; isImage: boolean }
  ) {
    this.isImage = data.isImage;
  }
}