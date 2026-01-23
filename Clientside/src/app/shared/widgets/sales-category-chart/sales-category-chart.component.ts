import {
  Component,
  Input,
  ChangeDetectionStrategy,
  OnChanges,
  OnDestroy
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
export class SalesCategoryChartComponent implements OnChanges, OnDestroy {

  @Input() labels: string[] = [];
  @Input() values: number[] = [];

  chart?: ApexCharts;
  themeObserver?: MutationObserver;

  ngOnChanges(): void {
    if (!this.labels.length || !this.values.length) return;
    this.render();
    this.watchTheme();
  }

  ngOnDestroy(): void {
    this.chart?.destroy();
    this.themeObserver?.disconnect();
  }

  /* =========================
     THEME WATCHER
  ========================= */

  private watchTheme(): void {
    if (this.themeObserver) return;

    this.themeObserver = new MutationObserver(() => {
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
    this.chart?.destroy();

    const isDark = document.body.classList.contains('dark');
    const total = this.values.reduce((a, b) => a + b, 0);

    // Text contrast — explicit per theme
    const TEXT_DARK = '#f1f5f9';
    const MUTED_DARK = '#cbd5e1';

    const TEXT_LIGHT = '#111827';
    const MUTED_LIGHT = '#374151';

    this.chart = new ApexCharts(
      document.querySelector('#sales-category-chart')!,
      {
        chart: {
          type: 'donut',
          height: 320,
          foreColor: isDark ? TEXT_DARK : TEXT_LIGHT
        },

        labels: this.labels,
        series: this.values,

        plotOptions: {
          pie: {
            donut: {
              size: '68%'
            }
          }
        },

        legend: {
          position: 'bottom',
          fontSize: '12px',
          labels: {
            colors: isDark ? MUTED_DARK : MUTED_LIGHT
          },
          formatter: (label: any, opts: { w: { globals: { series: { [x: string]: any; }; }; }; seriesIndex: string | number; }) => {
            const val = opts.w.globals.series[opts.seriesIndex];
            const pct = total ? ((val / total) * 100).toFixed(1) : '0';
            return `${label} — ${pct}%`;
          }
        },

        dataLabels: {
          enabled: false
        },

        tooltip: {
          theme: isDark ? 'dark' : 'light',
          style: { fontSize: '12px' },
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