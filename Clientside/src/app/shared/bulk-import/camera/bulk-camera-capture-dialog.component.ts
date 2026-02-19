import {
  Component,
  Inject,
  OnDestroy,
  ViewChild,
  ElementRef,
  AfterViewInit
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

interface CameraDialogData {
  entityType: string;
  rowIndex: number;
}

@Component({
  standalone: true,
  selector: 'app-bulk-camera-capture-dialog',
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './bulk-camera-capture-dialog.component.html',
  styleUrls: ['./bulk-camera-capture-dialog.component.scss']
})
export class BulkCameraCaptureDialogComponent
  implements AfterViewInit, OnDestroy {

  @ViewChild('video') videoRef!: ElementRef<HTMLVideoElement>;
  @ViewChild('canvas') canvasRef!: ElementRef<HTMLCanvasElement>;

  stream?: MediaStream;
  captured: File[] = [];
  previews: { file: File; url: string }[] = [];

  usingFrontCamera = false;
  loading = false;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: CameraDialogData,
    private dialogRef: MatDialogRef<BulkCameraCaptureDialogComponent>,
    private snackbar: MatSnackBar
  ) {}

  async ngAfterViewInit() {
    if (!this.isCameraSupported()) {
      this.snackbar.open(
        'Camera not supported on this device. Use file upload instead.',
        'Close',
        { duration: 4000 }
      );
      return;
    }

    await this.startCamera();
  }

  isCameraSupported(): boolean {
    return !!(navigator.mediaDevices &&
              navigator.mediaDevices.getUserMedia);
  }

  async startCamera() {

    if (this.stream) {
      this.stop();
    }

    this.stream = await navigator.mediaDevices.getUserMedia({
      video: {
        facingMode: this.usingFrontCamera ? 'user' : 'environment'
      }
    });

    const video = this.videoRef.nativeElement;
    video.srcObject = this.stream;
    await video.play();
  }

  async toggleCamera() {
    this.usingFrontCamera = !this.usingFrontCamera;
    await this.startCamera();
  }

  capture() {

    if (this.loading) return;

    this.loading = true;

    this.snackbar.open(
      'Photo captured. Processing...',
      undefined,
      { duration: 1500 }
    );

    const video = this.videoRef.nativeElement;
    const canvas = this.canvasRef.nativeElement;

    canvas.width = video.videoWidth;
    canvas.height = video.videoHeight;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    ctx.drawImage(video, 0, 0);

    canvas.toBlob(blob => {

      if (!blob) {
        this.loading = false;
        return;
      }

      const timestamp = Date.now();
      const fileName =
        `${this.data.entityType}-${this.data.rowIndex}-${timestamp}.jpg`;

      const file = new File([blob], fileName, {
        type: 'image/jpeg'
      });

      const url = window.URL.createObjectURL(file);

      this.captured.push(file);
      this.previews.push({ file, url });

      this.loading = false;

    }, 'image/jpeg', 1.0);
  }

  remove(index: number) {

    const preview = this.previews[index];

    if (preview) {
      window.URL.revokeObjectURL(preview.url);
    }

    this.previews.splice(index, 1);
    this.captured.splice(index, 1);
  }

  confirm() {
    this.stop();
    this.dialogRef.close(this.captured);
  }

  cancel() {
    this.stop();
    this.dialogRef.close(null);
  }

  stop() {
    this.stream?.getTracks().forEach(t => t.stop());
  }

  ngOnDestroy() {
    this.stop();
    this.previews.forEach(p =>
      window.URL.revokeObjectURL(p.url)
    );
  }
}