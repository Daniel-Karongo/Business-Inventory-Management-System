import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd, RouterLink } from '@angular/router';
import { CommonModule } from '@angular/common';
import { filter } from 'rxjs';

@Component({
  selector: 'app-breadcrumb',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './breadcrumb.component.html',
  styleUrls: ['./breadcrumb.component.scss']
})
export class BreadcrumbComponent implements OnInit {

  segments: { label: string, url: string }[] = [];

  constructor(private router: Router) {}

  ngOnInit() {
    this.router.events.pipe(filter(e => e instanceof NavigationEnd))
      .subscribe(() => this.buildBreadcrumb());
  }

  buildBreadcrumb() {
    const urlSegments = this.router.url.split('/').filter(x => x);
    let url = '';
    this.segments = urlSegments.map(seg => {
      url += '/' + seg;
      return { label: this.format(seg), url };
    });
  }

  format(seg: string) {
    return seg.replace(/-/g, ' ').replace(/\b\w/g, c => c.toUpperCase());
  }
}