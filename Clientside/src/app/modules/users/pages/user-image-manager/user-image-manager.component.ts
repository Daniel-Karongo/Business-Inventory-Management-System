import {
  Component,
  Input,
  OnInit,
  ViewChild,
  ElementRef
} from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { UserService } from '../../services/user/user.service';
import { UserImage, UserImageAudit } from '../../models/user.model';
import { MatDialog } from '@angular/material/dialog';
import { FileViewerDialog } from '../../../../shared/components/file-viewer/file-viewer.component';
import { ImageUploadDialogComponent } from
  '../../../../shared/components/image-upload-dialog/image-upload-dialog.component';
import { MatTooltipModule } from '@angular/material/tooltip';


@Component({
  selector: 'app-user-image-manager',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule,
    MatTooltipModule
  ],
  templateUrl: './user-image-manager.component.html',
  styleUrls: ['./user-image-manager.component.scss']
})
export class UserImageManagerComponent implements OnInit {

  @Input() userId!: string;

  images: UserImage[] = [];
  uploading = false;
  loading = true;
  imageUrlMap = new Map<string, string>();
  files: File[] = [];
  previews: { src?: string; name: string; type: string }[] = [];
  imageAudits: UserImageAudit[] = [];
  loadingAudits = false;


  constructor(
    private userService: UserService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
  ) { }

  ngOnInit(): void {
    this.loadImages();
    this.loadImageAudits();
  }

  loadImageAudits() {
    this.loadingAudits = true;

    this.userService.imageAuditTarget(this.userId).subscribe({
      next: audits => {
        this.imageAudits = audits ?? [];
        this.loadingAudits = false;
      },
      error: () => {
        this.loadingAudits = false;
      }
    });
  }

  /* ================= LOAD ================= */

  loadImages() {
    this.loading = true;

    this.userService.listImages(this.userId).subscribe({
      next: imgs => {
        this.images = imgs || [];

        // Fetch blobs for image files
        this.images
          .filter(i => i.image)
          .forEach(img => this.loadImageBlob(img));

        this.loading = false;
      },
      error: () => {
        this.snackbar.open('Failed to load images', 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  private loadImageBlob(img: UserImage) {
    this.userService.getUserImageBlob(this.userId, img.fileName).subscribe(blob => {
      const url = URL.createObjectURL(blob);
      this.imageUrlMap.set(img.fileName, url);
    });
  }

  imageUrl(img: UserImage): string {
    return this.imageUrlMap.get(img.fileName) ?? '';
  }

  openPdf(img: UserImage) {
    this.userService
      .getUserImageBlob(this.userId, img.fileName)
      .subscribe(blob => {
        const url = URL.createObjectURL(blob);
        window.open(url, '_blank');
      });
  }

  viewImage(img: UserImage) {
    const src = this.imageUrl(img);

    this.dialog.open(FileViewerDialog, {
      data: {
        preview: {
          src,
          name: img.fileName,
          type: img.pdf ? 'application/pdf' : 'image'
        }
      },
      width: '80%',
      maxWidth: '1100px'
    });
  }

  /* ================= UPLOAD ================= */

  triggerUpload() {
    this.dialog.open(ImageUploadDialogComponent, {
      width: '420px'
    })
      .afterClosed()
      .subscribe(result => {
        if (!result) return;

        this.uploading = true;

        this.userService
          .uploadImages(this.userId, [result])
          .subscribe({
            next: () => {
              this.uploading = false;
              this.loadImages();
              this.snackbar.open('Image uploaded', 'Close', { duration: 2000 });
            },
            error: () => {
              this.uploading = false;
              this.snackbar.open('Upload failed', 'Close', { duration: 3000 });
            }
          });
      });
  }

  /* ================= DELETE / RESTORE ================= */

  onPrimaryAction(img: UserImage) {
    if (!img.deleted) {
      // ACTIVE → SOFT DELETE
      this.userService.softDeleteImage(this.userId, img.fileName).subscribe({
        next: () => {
          this.snackbar.open('Image deleted', 'Close', { duration: 2000 });
          this.loadImages();
          this.loadImageAudits();
        },
        error: () =>
          this.snackbar.open('Delete failed', 'Close', { duration: 3000 })
      });
    } else {
      // SOFT-DELETED → RESTORE
      this.userService.restoreImage(this.userId, img.fileName).subscribe({
        next: () => {
          this.snackbar.open('Image restored', 'Close', { duration: 2000 });
          this.loadImages();
          this.loadImageAudits();
        },
        error: () =>
          this.snackbar.open('Restore failed', 'Close', { duration: 3000 })
      });
    }
  }

  onHardDelete(img: UserImage) {
    this.userService.hardDeleteImage(this.userId, img.fileName).subscribe({
      next: () => {
        this.snackbar.open('Image permanently deleted', 'Close', { duration: 2000 });
        this.loadImages();
        this.loadImageAudits();
      },
      error: () =>
        this.snackbar.open('Hard delete failed', 'Close', { duration: 3000 })
    });
  }
}