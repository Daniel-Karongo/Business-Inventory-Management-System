import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-stat-card',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './stat-card.component.html',
  styleUrls: ['./stat-card.component.scss']
})
export class StatCardComponent {
  @Input() title = '';
  @Input() value = '';
  @Input() variance?: number;

  get varianceClass(): string {
    if (this.variance == null) return '';
    return this.variance >= 0 ? 'positive' : 'negative';
  }

  get varianceSymbol(): string {
    if (this.variance == null) return '';
    return this.variance >= 0 ? '↑' : '↓';
  }
}