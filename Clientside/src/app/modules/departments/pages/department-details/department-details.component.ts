import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';

import { DepartmentService } from '../../services/department.service';
import { DepartmentDTO } from '../../models/department.model';

@Component({
  standalone: true,
  selector: 'app-department-details',
  imports: [CommonModule, RouterModule, MatButtonModule],
  templateUrl: './department-details.component.html',
  styleUrls: ['./department-details.component.scss']
})
export class DepartmentDetailsComponent implements OnInit {

  department?: DepartmentDTO;

  constructor(
    private route: ActivatedRoute,
    private service: DepartmentService
  ) { }

  ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id')!;
    this.service.get(id).subscribe(d => this.department = d);
  }

  get branchNames(): string {
    if (!this.department?.branches || this.department.branches.length === 0) {
      return '—';
    }
    return this.department.branches.map(b => b.name).join(', ');
  }
}