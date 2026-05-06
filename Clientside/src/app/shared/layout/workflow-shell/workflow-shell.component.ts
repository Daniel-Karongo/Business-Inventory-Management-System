import {
  ChangeDetectionStrategy,
  Component,
  Input
} from '@angular/core';

import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-workflow-shell',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './workflow-shell.component.html',
  styleUrls: ['./workflow-shell.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class WorkflowShellComponent {

  @Input()
  fluid = false;

}