import {
  Component,
  Input,
  Output,
  EventEmitter,
  ViewChild,
  ElementRef
} from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatTooltipModule } from '@angular/material/tooltip';
import { BulkImportScrollService } from '../base/bulk-import-scroll.service';

@Component({
  standalone: true,
  selector: 'app-bulk-import-shell',
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatTooltipModule
  ],
  templateUrl: './bulk-import-shell.component.html',
  styleUrls: ['./bulk-import-shell.component.scss']
})
export class BulkImportShellComponent {

  @ViewChild('scrollContainer', { static: true })
  sc!: ElementRef<HTMLElement>;

  @ViewChild('bottomAnchor', { static: true })
  ba!: ElementRef<HTMLElement>;

  @Input() loading = false;              // hard block
  @Input() loadingLabel?: string;        // “Importing sales…”
  @Input() progress?: number | null;     // optional %


  constructor(private scroll: BulkImportScrollService) { }

  ngAfterViewInit(): void {
    this.scroll.register(this.sc, this.ba);
  }

  @Input() title = 'Bulk Import';
  @Input() rowCount = 0;
  @Input() submitting = false;
  @Input() formInvalid = false;
  @Input() supportsArchiveImport = false;

  @Output() importExcel = new EventEmitter<File>();
  @Output() importCsv = new EventEmitter<File>();
  @Output() downloadCsv = new EventEmitter<void>();
  @Output() downloadExcel = new EventEmitter<void>();
  @Output() submit = new EventEmitter<void>();
  @Output() cancel = new EventEmitter<void>();

  @Output() goToLine = new EventEmitter<number>();
  @Output() goTop = new EventEmitter<void>();
  @Output() goBottom = new EventEmitter<void>();
  @Output() prevError = new EventEmitter<void>();
  @Output() nextError = new EventEmitter<void>();
  @Output() clearAll = new EventEmitter<void>();
  @Output() exportExcel = new EventEmitter<void>();
  @Output() exportCsv = new EventEmitter<void>();
  @Output() zipDropped = new EventEmitter<File>();

  onGo(line?: number) {
    if (line && line > 0) {
      this.goToLine.emit(line);
    }
  }

  isDragging = false;

  onDragOver(event: DragEvent) {
    event.preventDefault();
    this.isDragging = true;
  }

  onDragLeave() {
    this.isDragging = false;
  }

  onDrop(event: DragEvent) {
    event.preventDefault();
    this.isDragging = false;

    const file = event.dataTransfer?.files?.[0];
    if (!file) return;

    if (file.name.endsWith('.zip')) {
      this.zipDropped.emit(file);
    } else if (file.name.endsWith('.xlsx')) {
      this.importExcel.emit(file);
    } else if (file.name.endsWith('.csv')) {
      this.importCsv.emit(file);
    }
  }
}