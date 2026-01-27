import { Component, Input, OnInit, OnChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatDialog } from '@angular/material/dialog';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Observable } from 'rxjs';

import { FileViewerDialog } from '../file-viewer/file-viewer.component';
import { ImageUploadDialogComponent } from '../image-upload-dialog/image-upload-dialog.component';

export interface EntityImage {
  fileName: string;
  description?: string;
  deleted?: boolean;
  image?: boolean;
  pdf?: boolean;
}

export interface EntityImageAudit {
  fileName: string;
  action: 'UPLOAD' | 'DELETE' | 'RESTORE' | 'HARD_DELETE';
  performedByUsername: string;
  timestamp: string;
}

export interface EntityImageAdapter {
  listImages(id: string): Observable<EntityImage[]>;
  listImageAudits(id: string): Observable<EntityImageAudit[]>;

  getImageBlob(
    id: string,
    fileName: string,
    deleted?: boolean
  ): Observable<Blob>;

  uploadImages(
    id: string,
    files: { file: File; description: string }[]
  ): Observable<any>;

  softDeleteImage(id: string, fileName: string): Observable<any>;
  restoreImage(id: string, fileName: string): Observable<any>;
  hardDeleteImage(id: string, fileName: string): Observable<any>;
}

@Component({
  selector: 'app-entity-image-manager',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule
  ],
  templateUrl: './entity-image-manager.component.html',
  styleUrls: ['./entity-image-manager.component.scss']
})
export class EntityImageManagerComponent implements OnInit, OnChanges {

  @Input() entityId!: string;
  @Input() adapter!: EntityImageAdapter;
  @Input() allowHardDelete = false;
  @Input() readonly = false;

  images: EntityImage[] = [];
  audits: EntityImageAudit[] = [];

  loading = true;
  loadingAudits = false;
  uploading = false;

  imageUrlMap = new Map<string, string>();

  constructor(
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    if (this.entityId) {
      this.loadAll();
    }
  }

  ngOnChanges(): void {
    if (this.entityId) {
      this.loadAll();
    }
  }

  /* ================= LOAD ================= */

  private loadAll() {
    this.loadImages();
    this.loadAudits();
  }

  private loadImages() {
    this.loading = true;

    this.adapter.listImages(this.entityId).subscribe({
      next: imgs => {
        this.images = this.enrichTypes(imgs || []);

        this.images
          .filter(i => i.image)
          .forEach(i => this.loadBlob(i));

        this.loading = false;
      },
      error: () => {
        this.snackbar.open('Failed to load documents', 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  private loadAudits() {
    this.loadingAudits = true;
    console.log("Hello");
    console.log(this.entityId);

    this.adapter.listImageAudits(this.entityId).subscribe({
      next: audits => {
        console.log(audits);
        this.audits = audits ?? [];
        this.loadingAudits = false;
      },
      error: () => {
        this.loadingAudits = false;
      }
    });
  }

  private loadBlob(img: EntityImage) {
    this.adapter
      .getImageBlob(this.entityId, img.fileName, img.deleted)
      .subscribe(blob => {
        const url = URL.createObjectURL(blob);
        this.imageUrlMap.set(img.fileName, url);
      });
  }

  imageUrl(img: EntityImage): string {
    return this.imageUrlMap.get(img.fileName) ?? '';
  }

  private enrichTypes(imgs: EntityImage[]): EntityImage[] {
    return imgs.map(img => {
      const ext = img.fileName.split('.').pop()?.toLowerCase() ?? '';
      return {
        ...img,
        image: ['jpg', 'jpeg', 'png', 'gif', 'webp'].includes(ext),
        pdf: ext === 'pdf'
      };
    });
  }

  /* ================= VIEW ================= */

  view(img: EntityImage) {
    if (img.image) {
      const src = this.imageUrl(img);
      if (!src) return;

      this.dialog.open(FileViewerDialog, {
        data: {
          preview: {
            src,
            name: img.fileName,
            type: 'image'
          }
        },
        width: '80%',
        maxWidth: '1100px'
      });
      return;
    }

    if (img.pdf) {
      this.openPdf(img);
    }
  }

  private openPdf(img: EntityImage) {
    this.adapter
      .getImageBlob(this.entityId, img.fileName, img.deleted)
      .subscribe(blob => {
        const pdfBlob = new Blob([blob], { type: 'application/pdf' });
        const url = URL.createObjectURL(pdfBlob);
        window.open(url, '_blank');
        setTimeout(() => URL.revokeObjectURL(url), 60_000);
      });
  }

  /* ================= UPLOAD ================= */

  triggerUpload() {
    this.dialog.open(ImageUploadDialogComponent, { width: '420px' })
      .afterClosed()
      .subscribe(result => {
        if (!result) return;

        this.uploading = true;

        this.adapter.uploadImages(this.entityId, [result]).subscribe({
          next: () => {
            this.uploading = false;
            this.loadAll();
            this.snackbar.open('Document uploaded', 'Close', { duration: 2000 });
          },
          error: () => {
            this.uploading = false;
            this.snackbar.open('Upload failed', 'Close', { duration: 3000 });
          }
        });
      });
  }

  /* ================= ACTIONS ================= */

  primaryAction(img: EntityImage) {
    const action$ = img.deleted
      ? this.adapter.restoreImage(this.entityId, img.fileName)
      : this.adapter.softDeleteImage(this.entityId, img.fileName);

    action$.subscribe(() => {
      this.snackbar.open(
        img.deleted ? 'Document restored' : 'Document deleted',
        'Close',
        { duration: 2000 }
      );
      this.loadAll();
    });
  }

  hardDelete(img: EntityImage) {
    this.adapter.hardDeleteImage(this.entityId, img.fileName)
      .subscribe(() => {
        this.snackbar.open('Document permanently deleted', 'Close', { duration: 2000 });
        this.loadAll();
      });
  }
}