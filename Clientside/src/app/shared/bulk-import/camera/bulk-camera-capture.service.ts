import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { BulkCameraCaptureDialogComponent } from './bulk-camera-capture-dialog.component';

@Injectable({ providedIn: 'root' })
export class BulkCameraCaptureService {

  constructor(private dialog: MatDialog) {}

  async capture(entityType: string, rowIndex: number): Promise<File[] | null> {

    const ref = this.dialog.open(BulkCameraCaptureDialogComponent, {
      width: '95vw',
      maxWidth: '900px',
      height: '90vh',
      panelClass: 'enterprise-camera-dialog',
      data: { entityType, rowIndex }
    });

    return ref.afterClosed().toPromise();
  }
}