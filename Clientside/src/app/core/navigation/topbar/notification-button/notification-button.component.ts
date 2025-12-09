import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';

@Component({
  selector: 'app-notification-button',
  standalone: true,
  imports: [CommonModule, MatIconModule],
  templateUrl: './notification-button.component.html',
  styleUrls: ['./notification-button.component.scss']
})
export class NotificationButtonComponent {
  /** number displayed in the badge (0 hides the badge) */
  @Input() count = 0;

  /** emitted when the button is clicked */
  @Output() clicked = new EventEmitter<void>();

  onClick() {
    this.clicked.emit();
  }
}