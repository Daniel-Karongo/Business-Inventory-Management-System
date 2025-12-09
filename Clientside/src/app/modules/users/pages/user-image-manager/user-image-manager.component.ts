import { Component, Input, OnInit, ViewChild, ElementRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-user-image-manager',
  standalone: true,
  imports: [CommonModule, MatButtonModule, MatIconModule, MatSnackBarModule],
  templateUrl: './user-image-manager.component.html',
  styleUrls: ['./user-image-manager.component.scss']
})
export class UserImageManagerComponent implements OnInit {

  @Input() userId!: string;
  @ViewChild('fileInput') fileInput!: ElementRef<HTMLInputElement>;

  images: string[] = [];
  uploading = false;

  constructor(private userService: UserService, private snackbar: MatSnackBar) {}

  ngOnInit() {
    this.loadImages();
  }

  loadImages() {
    this.userService.listImages(this.userId).subscribe({
      next: res => this.images = res,
      error: () => this.snackbar.open('Failed to load images', 'Close', { duration: 3000 })
    });
  }

  triggerFile() {
    this.fileInput.nativeElement.click();
  }

  onFileSelected(event: any) {
    const file = event.target.files?.[0];
    if (!file) return;

    this.uploading = true;

    this.userService.uploadImage(this.userId, file).subscribe({
      next: () => {
        this.uploading = false;
        this.loadImages();
        this.fileInput.nativeElement.value = '';
        this.snackbar.open('Uploaded', 'Close', { duration: 2000 });
      },
      error: () => {
        this.uploading = false;
        this.snackbar.open('Upload failed', 'Close', { duration: 3000 });
      }
    });
  }

  deleteImage(fullUrl: string) {
    const filename = fullUrl.split('/').pop()!;
    this.userService.hardDeleteImage(this.userId, filename).subscribe({
      next: () => {
        this.snackbar.open('Deleted', 'Close', { duration: 2000 });
        this.loadImages();
      },
      error: () => this.snackbar.open('Delete failed', 'Close', { duration: 3000 })
    });
  }
}