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
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { BulkCameraCaptureService } from '../../bulk-import/camera/bulk-camera-capture.service';

(pdfjsLib as any).GlobalWorkerOptions.workerSrc =
  '/assets/pdf.worker.mjs';

export interface EntityImage {
  id?: string;
  fileName: string;
  deleted?: boolean;
  description?: string;
  image?: boolean;
  pdf?: boolean;
}

export interface EntityImageAuditVM extends EntityImageAudit {
  isDeleted?: boolean;
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
    deleted?: boolean,
    version?: number
  ): Observable<Blob>;

  uploadImages(
    id: string,
    files: {
      file: File;
      description: string;
      documentType?: string;
    }[]
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
    imageKey: string,
    reason?: string | null
  ): Observable<any>;

  hardDeleteImage(
    id: string,
    fileName: string,
    reason?: string | null
  ): Observable<any>;

  onThumbnailUpdated?: () => void;
  onChange?: () => void;

  deleteReasons?: string[];
  restoreReasons?: string[];
  hardDeleteReasons?: string[];
  supportsDescription?: boolean;
  entityLabel?: string;
  uploadMode?: 'image' | 'document';
  allowLogo?: boolean;
  descriptionOptions?: string[];
}

@Component({
  selector: 'app-entity-image-manager',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    FormsModule,
    MatProgressSpinnerModule
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
  audits: EntityImageAuditVM[] = [];

  loading = true;
  loadingAudits = false;
  uploading = false;

  imageUrlMap = new Map<string, string>();
  selectedFile: string | 'ALL' = 'ALL';
  selectedAction: string | 'ALL' = 'ALL';
  makingPrimary = false;
  private currentFileSet = new Set<string>();

  constructor(
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private camera: BulkCameraCaptureService
  ) { }

  ngOnInit() {
    if (
      this.entityId &&
      this.adapter
    ) {
      this.loadAll();
    }
  }

  ngOnChanges(
    changes: SimpleChanges
  ) {

    if (
      this.entityId &&
      this.adapter
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

  get entityLabel(): string {
    return this.adapter?.entityLabel ?? 'Image';
  }

  get entityLabelPlural(): string {
    return `${this.entityLabel}s`;
  }

  get uniqueFiles(): string[] {
    const auditFiles = this.audits.map(a => a.fileName);
    return Array.from(new Set(auditFiles));
  }

  get uniqueActions(): string[] {
    const actions = this.audits.map(a => a.action);
    return Array.from(new Set(actions));
  }

  get entityAuditScope(): EntityImageAuditVM[] {
    return [...this.audits].sort(
      (a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
    );
  }

  get filteredAudits(): EntityImageAuditVM[] {
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

  isFileDeleted(fileName: string): boolean {
    return !this.currentFileSet.has(fileName);
  }

  private loadAll() {

    if (!this.adapter) {
      return;
    }

    this.imageUrlMap.forEach(
      url => URL.revokeObjectURL(url)
    );

    this.imageUrlMap.clear();

    this.selectedFile = 'ALL';
    this.selectedAction = 'ALL';

    this.loading = true;
    this.loadingAudits = true;

    this.adapter.listImages(
      this.entityId
    ).subscribe({
      next: imgs => {
        this.images = this.enrichTypes(imgs || []);

        this.currentFileSet = new Set(
          this.images
            .filter(i => !i.deleted)
            .map(i => i.fileName)
        );

        // load previews
        this.images.forEach(img => {
          if (img.image) this.loadBlob(img);
          else if (img.pdf) this.generatePdfThumbnail(img.fileName);
        });

        this.loading = false;

        // ✅ NOW load audits AFTER fileSet is ready
        this.loadAudits();
      },
      error: (err) => {
        if (err?.status === 404) {
          this.images = [];
          this.currentFileSet = new Set();
          this.loading = false;
          this.loadAudits(); // still load audits
          return;
        }

        const message = err?.error?.message || 'Failed to load images';
        this.snackbar.open(message, 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  selectFile(fileName: string) {
    this.selectedFile =
      this.selectedFile === fileName ? 'ALL' : fileName;
  }

  private loadAudits() {
    this.loadingAudits = true;

    this.adapter.listImageAudits(this.entityId).subscribe({
      next: audits => {
        this.audits =
          (audits ?? []).map(a => ({
            ...a,
            isDeleted:
              this.isFileDeleted(
                a.fileName
              )
          }));

        this.loadingAudits = false;
      },
      error: () => {
        this.loadingAudits = false;
      }
    });
  }

  private loadBlob(img: EntityImage) {

    this.adapter
      .getImageBlob(
        this.entityId,
        img.fileName,
        img.deleted,
        Date.now()
      )
      .subscribe(blob => {

        const existing =
          this.imageUrlMap.get(
            img.fileName
          );

        if (existing) {
          URL.revokeObjectURL(
            existing
          );
        }

        const url =
          URL.createObjectURL(blob);

        this.imageUrlMap.set(
          img.fileName,
          url
        );

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
        panelClass:
          'enterprise-dialog',
        width: '90vw',
        maxWidth: '90vw',
        height: '90vh',
        maxHeight: '90vh',
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

  public openUploadDialog(): void {
    this.triggerUpload();
  }

  triggerUpload() {

    const beforeCount =
      this.images.length;

    this.dialog.open(
      ImageUploadDialogComponent,
      {
        width: '620px',
        maxWidth: '95vw',
        panelClass: 'enterprise-dialog',
        data: {
          uploadMode:
            this.adapter.uploadMode ?? 'image',

          supportsDescription:
            this.adapter.supportsDescription ?? false,

          descriptionOptions:
            this.adapter.descriptionOptions ?? []
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result) {
          return;
        }

        this.uploading = true;

        this.snackbar.open(
          'Uploading image...',
          undefined,
          {
            duration: 100000
          }
        );

        this.adapter
          .uploadImages(
            this.entityId,
            [result]
          )
          .subscribe({

            next: () => {

              this.snackbar.dismiss();

              this.snackbar.open(
                `${this.entityLabel} uploaded and being processed`,
                'Close',
                {
                  duration: 4000
                }
              );

              this.startUploadPolling(
                beforeCount
              );
            },

            error: () => {

              this.uploading = false;

              this.snackbar.dismiss();

              this.snackbar.open(
                'Upload failed',
                'Close',
                {
                  duration: 3000
                }
              );

            }

          });

      });

  }

  async capturePhotos() {

    const files =
      await this.camera.capture(
        this.entityLabel.toLowerCase(),
        0
      );

    if (!files?.length) {
      return;
    }

    this.uploading = true;

    const payload =
      files.map(file => ({
        file,
        description: '',
        documentType: 'OTHER'
      }));

    const beforeCount =
      this.images.length;

    this.adapter
      .uploadImages(
        this.entityId,
        payload
      )
      .subscribe({
        next: () => {

          this.uploading = false;

          this.snackbar.open(
            `${files.length} ${this.entityLabelPlural.toLowerCase()} captured and queued for processing`,
            'Close',
            {
              duration: 4000
            }
          );

          this.startUploadPolling(
            beforeCount
          );
        },
        error: () => {

          this.uploading = false;

          this.snackbar.open(
            'Camera upload failed',
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }

  private startUploadPolling(
    previousCount: number
  ) {

    let attempts = 0;

    const timer =
      setInterval(() => {

        attempts++;

        this.adapter
          .listImages(
            this.entityId
          )
          .subscribe(images => {

            const current =
              images ?? [];

            if (
              current.length >
              previousCount
            ) {

              clearInterval(timer);

              this.uploading = false;

              this.loadAll();

              if (
                this.adapter.onThumbnailUpdated
              ) {
                this.adapter.onThumbnailUpdated();
              }

              if (
                this.adapter.onChange
              ) {
                this.adapter.onChange();
              }

              this.snackbar.open(
                `${this.entityLabel} processing completed`,
                'Close',
                {
                  duration: 3000
                }
              );

              return;
            }

          });

        if (attempts >= 30) {
          clearInterval(timer);
        }

      }, 2000);

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

          if (this.adapter.onChange) {
            this.adapter.onChange();
          }
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

          if (this.adapter.onChange) {
            this.adapter.onChange();
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
        width: '500px',
        panelClass: 'enterprise-dialog',
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

          reasons:
            restoring
              ? this.adapter.restoreReasons ?? []
              : this.adapter.deleteReasons ?? [],

          requireReason: false,
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
              {
                duration: 3000
              }
            );

            this.loadAll();

            if (
              this.adapter.onChange
            ) {
              this.adapter.onChange();
            }
          },

          error: (err) => {

            const message =
              err?.error?.message ||
              err?.error ||
              err?.message ||
              (
                restoring
                  ? 'Restore failed'
                  : 'Delete failed'
              );

            this.snackbar.open(
              message,
              'Close',
              {
                duration: 5000
              }
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
        panelClass: 'enterprise-dialog',
        data: {
          title: 'Permanent Delete',

          message:
            'Permanently delete this document? This cannot be undone.',

          action: 'DELETE',

          confirmText: 'Delete Permanently',

          cancelText: 'Cancel',

          reasons:
            this.adapter.hardDeleteReasons ?? [],

          requireReason: false,

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
                {
                  duration: 4000
                }
              );

              this.loadAll();

              if (
                this.adapter.onChange
              ) {
                this.adapter.onChange();
              }
            },

            error: (err) => {

              const message =
                err?.error?.message ||
                err?.error ||
                err?.message ||
                'Permanent delete failed';

              this.snackbar.open(
                message,
                'Close',
                {
                  duration: 5000
                }
              );

            }
          });

      });

  }
}