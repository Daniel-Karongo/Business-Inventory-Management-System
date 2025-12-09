import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute } from '@angular/router';
import { UserService } from '../../services/user/user.service';
import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { User } from '../../models/user.model';

@Component({
  selector: 'app-user-details',
  standalone: true,
  imports: [CommonModule, MatCardModule, MatButtonModule],
  templateUrl: './user-details.component.html',
  styleUrls: ['./user-details.component.scss']
})
export class UserDetailsComponent implements OnInit {

  id?: string;
  user?: User;
  audits: any[] = [];
  images: string[] = [];

  constructor(
    private route: ActivatedRoute,
    private userService: UserService
  ) { }

  ngOnInit() {
    this.id = this.route.snapshot.paramMap.get('id') ?? undefined;
    if (this.id) {
      this.userService.get(this.id).subscribe(u => this.user = u);
      this.userService.auditsForUser(this.id).subscribe(a => this.audits = a);
      this.userService.listImages(this.id).subscribe(i => this.images = i);
    }
  }

  openImageManager() {
    // Could open a modal or route to a component; for now navigate to a fragment or show inline
    // For the sample we'll just console log — replace with your modal logic
    console.log('Open image manager for', this.id);
  }

  // helper to format branch names
  branchNames() {
    return (this.user?.branchHierarchy || []).map(b => b.branchName).join(', ');
  }
}