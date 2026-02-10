import {
  Component,
  Input,
  ChangeDetectionStrategy,
  OnChanges,
  OnDestroy,
  AfterViewInit,
  ViewChild,
  ElementRef
} from '@angular/core';
import { CommonModule } from '@angular/common';
import ApexCharts from 'apexcharts';

@Component({
  selector: 'app-sales-category-chart',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './sales-category-chart.component.html',
  styleUrls: ['./sales-category-chart.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class SalesCategoryChartComponent
  implements OnChanges, AfterViewInit, OnDestroy {

  @Input() labels: string[] = [];
  @Input() values: number[] = [];

  @ViewChild('wrapper', { static: true })
  wrapperRef!: ElementRef<HTMLDivElement>;

  chart?: ApexCharts;
  private resizeObserver?: ResizeObserver;
  private themeObserver?: MutationObserver;
  private viewReady = false;

  /* =========================
     LIFECYCLE
  ========================= */

  ngAfterViewInit(): void {
    this.viewReady = true;

    this.observeResize();
    this.watchTheme();

    if (this.labels.length && this.values.length) {
      this.render();
    }
  }

  ngOnChanges(): void {
    if (!this.viewReady || !this.labels.length || !this.values.length) return;
    this.render();
  }

  ngOnDestroy(): void {
    this.chart?.destroy();
    this.resizeObserver?.disconnect();
    this.themeObserver?.disconnect();
  }

  /* =========================
     OBSERVERS
  ========================= */

  private observeResize(): void {
    this.resizeObserver = new ResizeObserver(() => {
      this.render();
    });

    this.resizeObserver.observe(this.wrapperRef.nativeElement);
  }

  private watchTheme(): void {
    this.themeObserver = new MutationObserver(() => {
      // safest for Apex
      this.chart?.destroy();
      this.chart = undefined;
      this.render();
    });

    this.themeObserver.observe(document.body, {
      attributes: true,
      attributeFilter: ['class']
    });
  }

  /* =========================
     RENDER
  ========================= */

  private render(): void {
    if (!this.wrapperRef) return;

    this.chart?.destroy();

    const containerWidth = this.wrapperRef.nativeElement.clientWidth;
    const isCompact = containerWidth < 420;
    const isDark = document.body.classList.contains('dark');

    const total = this.values.reduce((a, b) => a + b, 0);

    this.chart = new ApexCharts(
      this.wrapperRef.nativeElement.querySelector('#sales-category-chart')!,
      {
        chart: {
          type: 'donut',
          height: isCompact ? 240 : 300,
          foreColor: isDark ? '#f1f5f9' : '#111827'
        },

        labels: this.labels,
        series: this.values,

        plotOptions: {
          pie: {
            donut: {
              size: isCompact ? '72%' : '68%'
            }
          }
        },

        legend: {
          position: isCompact ? 'bottom' : 'right',
          horizontalAlign: isCompact ? 'center' : 'left',
          fontSize: '12px',
          offsetY: isCompact ? 6 : 0,
          labels: {
            colors: isDark ? '#cbd5e1' : '#374151'
          },
          formatter: (label: any, opts: { w: { globals: { series: { [x: string]: any; }; }; }; seriesIndex: string | number; }) => {
            const val = opts.w.globals.series[opts.seriesIndex];
            const pct = total ? ((val / total) * 100).toFixed(1) : '0';
            return `${label} — ${pct}%`;
          }
        },

        dataLabels: { enabled: false },

        tooltip: {
          theme: isDark ? 'dark' : 'light',
          y: {
            formatter: (v: number) => {
              const pct = total ? ((v / total) * 100).toFixed(1) : '0';
              return `KSh ${v.toLocaleString('en-KE')} (${pct}%)`;
            }
          }
        },

        stroke: {
          width: 1,
          colors: isDark ? ['#1f2937'] : ['#ffffff']
        },

        colors: [
          '#2563eb',
          '#16a34a',
          '#f59e0b',
          '#ef4444',
          '#7c3aed'
        ]
      }
    );

    this.chart.render();
  }
}