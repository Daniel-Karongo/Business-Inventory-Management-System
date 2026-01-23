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
  selector: 'app-revenue-line-chart',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './revenue-line-chart.component.html',
  styleUrls: ['./revenue-line-chart.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class RevenueLineChartComponent implements OnChanges, OnDestroy {

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

  private formatDate(d: string): string {
    return new Date(d).toLocaleDateString('en-GB'); // dd-MM-yyyy
  }

  private render(): void {
    this.chart?.destroy();

    const isDark = document.body.classList.contains('dark');

    // Text contrast — explicit per theme
    const TEXT_DARK = '#f1f5f9';
    const MUTED_DARK = '#cbd5e1';

    const TEXT_LIGHT = '#111827';
    const MUTED_LIGHT = '#4b5563';

    this.chart = new ApexCharts(
      document.querySelector('#revenue-line-chart')!,
      {
        chart: {
          type: 'area',
          height: 320,
          toolbar: { show: false },
          zoom: { enabled: false },
          foreColor: isDark ? TEXT_DARK : TEXT_LIGHT
        },

        series: [
          { name: 'Revenue', data: this.values }
        ],

        xaxis: {
          categories: this.labels.map(d => this.formatDate(d)),
          labels: {
            rotate: -45,
            style: {
              fontSize: '11px',
              colors: isDark ? MUTED_DARK : MUTED_LIGHT
            }
          },
          axisBorder: { show: false },
          axisTicks: { show: false }
        },

        yaxis: {
          labels: {
            formatter: (v: { toLocaleString: (arg0: string) => any; }) => `KSh ${v.toLocaleString('en-KE')}`,
            style: {
              fontSize: '11px',
              colors: isDark ? MUTED_DARK : MUTED_LIGHT
            }
          }
        },

        stroke: {
          curve: 'smooth',
          width: 2.5
        },

        fill: {
          type: 'gradient',
          gradient: {
            shadeIntensity: 0.15,
            opacityFrom: 0.28,
            opacityTo: 0.02
          }
        },

        grid: {
          borderColor: isDark ? '#475569' : '#d1d5db',
          strokeDashArray: 3
        },

        tooltip: {
          theme: isDark ? 'dark' : 'light',
          style: { fontSize: '12px' },
          marker: { show: false },
          y: {
            formatter: (v: { toLocaleString: (arg0: string) => any; }) => `KSh ${v.toLocaleString('en-KE')}`
          }
        },

        colors: ['#3b82f6']
      }
    );

    this.chart.render();
  }
}