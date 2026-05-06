import {
  ChangeDetectionStrategy,
  Component,
  Input
} from '@angular/core';

import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-workflow-card',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './workflow-card.component.html',
  styleUrls: ['./workflow-card.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class WorkflowCardComponent {

  @Input()
  padded = true;

}