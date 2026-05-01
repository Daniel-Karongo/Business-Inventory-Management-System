import { CommonModule } from '@angular/common';
import { Component, Input, OnChanges, OnInit, SimpleChanges } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Observable } from 'rxjs';

import { FormsModule } from '@angular/forms';
import { firstValueFrom } from 'rxjs';
import { FileViewerDialog } from '../file-viewer/file-viewer.component';
import { ImageUploadDialogComponent } from '../image-upload-dialog/image-upload-dialog.component';
import { ReasonDialogComponent } from '../reason-dialog/reason-dialog.component';

import * as pdfjsLib from 'pdfjs-dist';
import { RenameDeviceDialogComponent } from '../rename-device-dialog/rename-device-dialog.component';

// 👇 THIS is the fix
(pdfjsLib as any).GlobalWorkerOptions.workerSrc =
  '/assets/pdf.worker.mjs';

export interface EntityImage {
  fileName: string;
  description?: string;
  deleted?: boolean;
  image?: boolean;
  pdf?: boolean;
}

export type EntityImageAuditAction =
  | 'UPLOAD'
  | 'SOFT_DELETE'
  | 'DELETE'
  | 'RESTORE'
  | 'RETRIEVE'
  | 'DOWNLOAD'
  | 'SOFT_DELETE_ALL'
  | 'DELETE_ALL'
  | 'RESTORE_ALL';

export interface EntityImageAudit {
  fileName: string;
  action: EntityImageAuditAction | string; // tolerant of future backend additions
  reason?: string | null;
  performedByUsername: string;
  timestamp: string;
}

const ACTION_LABELS: Record<string, string> = {
  SOFT_DELETE: 'Soft Delete',
  DELETE: 'Hard Delete',
  RESTORE: 'Restore',
  RETRIEVE: 'Retrieve',
  DOWNLOAD: 'Download',
  UPLOAD: 'Upload',
  UPDATE_DESCRIPTION: 'Update Description'
};

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

  setProfileThumbnail?(
    id: string,
    fileName: string
  ): Observable<any>;

  updateDescription?: (
    id: string,
    fileName: string,
    description: string
  ) => Observable<any>;

  softDeleteImage(
    id: string,
    fileName: string,
    reason?: string | null
  ): Observable<any>;

  restoreImage(
    id: string,
    fileName: string,
    reason?: string | null
  ): Observable<any>;

  hardDeleteImage(
    id: string,
    fileName: string,
    reason?: string | null
  ): Observable<any>;

  onThumbnailUpdated?: () => void;
}

@Component({
  selector: 'app-entity-image-manager',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    FormsModule
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
  selectedFile: string | 'ALL' = 'ALL';
  selectedAction: string | 'ALL' = 'ALL';
  makingPrimary = false;

  constructor(
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit() {
    if (this.entityId) {
      this.loadAll();
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (
      changes['entityId'] &&
      !changes['entityId'].firstChange &&
      this.entityId
    ) {
      this.loadAll();
    }
  }

  ngOnDestroy(): void {
    this.imageUrlMap.forEach(url =>
      URL.revokeObjectURL(url)
    );
  }

  /* ================= LOAD ================= */

  get uniqueFiles(): string[] {
    return this.images.map(i => i.fileName);
  }

  get uniqueActions(): string[] {
    const actions = this.audits.map(a => a.action);
    return Array.from(new Set(actions));
  }

  get entityAuditScope(): EntityImageAudit[] {
    const fileSet = new Set(this.images.map(i => i.fileName));
    return this.audits.filter(a => fileSet.has(a.fileName));
  }

  get filteredAudits(): EntityImageAudit[] {
    return this.entityAuditScope.filter(a => {

      const fileMatch =
        this.selectedFile === 'ALL' ||
        a.fileName === this.selectedFile;

      const actionMatch =
        this.selectedAction === 'ALL' ||
        a.action === this.selectedAction;

      return fileMatch && actionMatch;
    });
  }

  private loadAll() {
    this.selectedFile = 'ALL';
    this.selectedAction = 'ALL';
    this.loadImages();
    this.loadAudits();
  }

  selectFile(fileName: string) {
    this.selectedFile =
      this.selectedFile === fileName ? 'ALL' : fileName;
  }

  private loadImages() {
    this.loading = true;

    this.adapter.listImages(this.entityId).subscribe({
      next: imgs => {
        this.images = this.enrichTypes(imgs || []);

        this.images.forEach(img => {
          if (img.image) {
            this.loadBlob(img);
          } else if (img.pdf) {
            this.generatePdfThumbnail(img.fileName);
          }
        });

        this.loading = false;
      },
      error: (err) => {

        // 🚫 suppress "User not found" for deleted users
        if (err?.status === 404) {
          this.images = [];
          this.loading = false;
          return;
        }

        // ✅ only show real errors
        const message = err?.error?.message || 'Failed to load images';
        this.snackbar.open(message, 'Close', { duration: 3000 });
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

  displayAction(action: string): string {
    return ACTION_LABELS[action] ??
      action.toLowerCase().replace(/_/g, ' ')
        .replace(/\b\w/g, c => c.toUpperCase());
  }

  async generatePdfThumbnail(fileName: string) {
    try {
      const blob = await firstValueFrom(
        this.adapter.getImageBlob(this.entityId, fileName)
      );

      if (!blob) return;

      const pdf = await pdfjsLib.getDocument({
        data: await blob.arrayBuffer()
      }).promise;

      const page = await pdf.getPage(1);

      const viewport = page.getViewport({ scale: 0.5 });

      const canvas = document.createElement('canvas');
      const context = canvas.getContext('2d');

      if (!context) return;

      canvas.width = viewport.width;
      canvas.height = viewport.height;

      await page.render({
        canvasContext: context,
        viewport
      } as any).promise;

      const url = canvas.toDataURL();
      this.imageUrlMap.set(fileName, url);

    } catch (e) {
      console.error('PDF thumbnail failed:', fileName, e);
    }
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

  editDescription(img: EntityImage) {

    if (!this.adapter.updateDescription) return;

    const ref = this.dialog.open(RenameDeviceDialogComponent, {
      width: '520px',
      data: {
        currentName: img.description || '',
        title: 'Edit Document Description',
        subtitle: 'Update the description for this document.',
        label: 'Description'
      }
    });

    ref.afterClosed().subscribe(result => {

      if (!result) return;

      this.adapter.updateDescription!(
        this.entityId,
        img.fileName,
        result
      ).subscribe({
        next: () => {

          this.snackbar.open(
            'Description updated',
            'Close',
            { duration: 2500 }
          );

          this.loadAll();
        },
        error: () => {
          this.snackbar.open(
            'Failed to update description',
            'Close',
            { duration: 3000 }
          );
        }
      });

    });
  }

  makePrimary(img: EntityImage) {

    if (!this.adapter.setProfileThumbnail) return;

    this.makingPrimary = true;

    this.adapter
      .setProfileThumbnail(this.entityId, img.fileName)
      .subscribe({
        next: () => {
          this.snackbar.open(
            'Profile thumbnail updated',
            'Close',
            { duration: 3000 }
          );

          this.loadAll();

          if (this.adapter.onThumbnailUpdated) {
            this.adapter.onThumbnailUpdated();
          }

          this.makingPrimary = false;
        },
        error: (err) => {
          const msg =
            err?.error?.message ||
            err?.error?.text ||
            'Failed to update thumbnail';

          this.snackbar.open(msg, 'Close', { duration: 4000 });
          this.makingPrimary = false;
        }
      });
  }

  /* ================= ACTIONS ================= */

  primaryAction(img: EntityImage) {

    const restoring = !!img.deleted;

    const ref = this.dialog.open(
      ReasonDialogComponent,
      {
        width: '480px',
        data: {
          title: restoring
            ? 'Restore Document'
            : 'Soft Delete Document',

          message: restoring
            ? 'Restore this document and make it active again?'
            : 'Soft-delete this document? It can be restored later.',

          action: restoring
            ? 'RESTORE'
            : 'DELETE',

          confirmText: restoring
            ? 'Restore'
            : 'Delete',

          cancelText: 'Cancel',

          reasons: [
            'Duplicate',
            'Outdated',
            'Incorrect file',
            'Administrative cleanup'
          ],

          allowCustomReason: true
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        const reason = result.reason ?? null;

        const action$ = restoring
          ? this.adapter.restoreImage(
            this.entityId,
            img.fileName,
            reason
          )
          : this.adapter.softDeleteImage(
            this.entityId,
            img.fileName,
            reason
          );

        action$.subscribe({
          next: () => {

            this.snackbar.open(
              restoring
                ? 'Document restored'
                : 'Document deleted (recoverable)',
              'Close',
              { duration: 3000 }
            );

            this.loadAll();
          },

          error: () => {

            this.snackbar.open(
              restoring
                ? 'Restore failed'
                : 'Delete failed',
              'Close',
              { duration: 4000 }
            );

          }
        });

      });

  }

  hardDelete(img: EntityImage) {

    const ref = this.dialog.open(
      ReasonDialogComponent,
      {
        width: '500px',
        data: {
          title: 'Permanent Delete',

          message:
            'Permanently delete this document? This cannot be undone.',

          action: 'DELETE',

          confirmText: 'Delete Permanently',

          cancelText: 'Cancel',

          reasons: [
            'Legal retention expired',
            'Corrupted file',
            'Duplicate permanent purge',
            'Security cleanup'
          ],

          allowCustomReason: true
        }
      }
    );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.adapter.hardDeleteImage(
          this.entityId,
          img.fileName,
          result.reason ?? null
        )
          .subscribe({
            next: () => {

              this.snackbar.open(
                'Document permanently deleted',
                'Close',
                { duration: 4000 }
              );

              this.loadAll();
            },

            error: () => {

              this.snackbar.open(
                'Permanent delete failed',
                'Close',
                { duration: 4000 }
              );

            }
          });

      });

  }
}