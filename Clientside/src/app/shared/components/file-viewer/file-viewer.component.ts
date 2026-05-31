import { CommonModule } from '@angular/common';
import { Component, Inject } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  selector: 'app-file-viewer',
  standalone: true,
  templateUrl: './file-viewer.component.html',
  styleUrls: ['./file-viewer.component.scss'],
  imports: [
    CommonModule,
    MatDialogModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule
  ]
})
export class FileViewerDialog {

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: {
      preview?: {
        src?: string;
        name?: string;
        type?: string;
      };
      file?: File;
    },
    private dialogRef:
      MatDialogRef<FileViewerDialog>
  ) {
  }

  isImage(): boolean {

    return !!this.data.preview?.src;
  }

  openExternal(): void {

    if (this.data.preview?.src) {

      window.open(
        this.data.preview.src,
        '_blank',
        'noopener,noreferrer'
      );

      return;
    }

    if (this.data.file) {

      const url =
        URL.createObjectURL(
          this.data.file
        );

      window.open(
        url,
        '_blank'
      );
    }
  }

  close(): void {

    this.dialogRef.close();
  }
}