import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BudgetEditorComponent } from '../../components/budget-editor/budget-editor.component';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';

@Component({
  standalone: true,
  selector: 'app-budget-dashboard',
  imports: [CommonModule, MatCardModule, BudgetEditorComponent, MatFormFieldModule],
  templateUrl: './budget-dashboard.component.html',
  styleUrls: ['./budget-dashboard.component.scss']
})
export class BudgetDashboardComponent {}