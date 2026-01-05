import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router, NavigationEnd, RouterModule } from '@angular/router';
import { filter } from 'rxjs';

@Component({
  selector: 'app-breadcrumb',
  standalone: true,
  templateUrl: './breadcrumb.component.html',
  styleUrls: ['./breadcrumb.component.scss'],
  imports: [
    RouterModule,
    CommonModule
  ]
})
export class BreadcrumbComponent implements OnInit {

  segments: { label: string; url: any[] }[] = [];

  constructor(
    private router: Router,
    private route: ActivatedRoute
  ) { }

  ngOnInit() {
    this.buildBreadcrumb();

    this.router.events
      .pipe(filter(e => e instanceof NavigationEnd))
      .subscribe(() => this.buildBreadcrumb());
  }

  buildBreadcrumb() {
    const segments: { label: string; url: any[] }[] = [];
    let currentRoute: ActivatedRoute | null = this.route.root;
    let url: string[] = [];

    while (currentRoute !== null) {

      const params = currentRoute.snapshot.params;

      currentRoute.snapshot.url.forEach(seg => {
        url.push(seg.path);

        // 🔑 Only use param IF this segment matches the param value
        const paramKey = Object.keys(params).find(
          key => params[key] === seg.path
        );

        const label = paramKey
          ? decodeURIComponent(params[paramKey])
          : this.format(seg.path);

        segments.push({
          label,
          url: [...url]
        });
      });

      currentRoute = currentRoute.firstChild;
    }

    this.segments = segments;
  }

  private format(seg: string) {
    return decodeURIComponent(seg)
      .replace(/-/g, ' ')
      .replace(/\b\w/g, c => c.toUpperCase());
  }
}