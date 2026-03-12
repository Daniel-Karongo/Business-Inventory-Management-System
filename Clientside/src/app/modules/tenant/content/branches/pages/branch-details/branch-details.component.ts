import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';

import { BranchService } from '../../services/branch.service';
import { BranchDTO } from '../../models/branch.model';

@Component({
  standalone: true,
  selector: 'app-branch-details',
  imports: [CommonModule, RouterModule, MatButtonModule],
  templateUrl: './branch-details.component.html'
})
export class BranchDetailsComponent implements OnInit {

  branch?: BranchDTO;

  constructor(
    private route: ActivatedRoute,
    private service: BranchService
  ) {}

  ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id')!;
    this.service.getById(id).subscribe(b => this.branch = b);
  }
}