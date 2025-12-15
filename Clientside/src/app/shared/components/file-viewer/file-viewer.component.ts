import { CommonModule } from "@angular/common";
import { Component, Inject } from "@angular/core";
import { MatButtonModule } from "@angular/material/button";
import { MAT_DIALOG_DATA, MatDialog, MatDialogModule } from "@angular/material/dialog";
import { MatIconModule } from "@angular/material/icon";

/* ============================================================
   FILE VIEWER DIALOG
============================================================ */
@Component({
  selector: 'app-file-viewer',
  standalone: true,
  templateUrl: './file-viewer.component.html',
  styleUrls: ['./file-viewer.component.scss'],
  imports: [
    CommonModule,
    MatDialogModule, 
    MatIconModule, 
    MatButtonModule
  ]
})
export class FileViewerDialog {

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: {
      preview?: { src?: string; name?: string; type?: string };
      file?: File;
    },
    private dialog: MatDialog
  ) {}

  isImage(): boolean {
    return !!this.data.preview?.src;
  }

  openPdf() {
    if (this.data.preview?.src) {
      window.open(this.data.preview.src, '_blank');
      return;
    }

    if (this.data.file) {
      const url = URL.createObjectURL(this.data.file);
      window.open(url, '_blank');
    }
  }

  close() {
    this.dialog.closeAll();
  }
}