import {
  Component,
  Input,
  ChangeDetectionStrategy,
  OnChanges,
  OnDestroy,
  AfterViewInit
} from '@angular/core';
import { CommonModule } from '@angular/common';
import ApexCharts from 'apexcharts';
import { observeThemeChange } from '../../../core/utils/theme-observer';
import { DateUtilsService } from '../../../core/services/date-utils';

@Component({
  selector: 'app-revenue-line-chart',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './revenue-line-chart.component.html',
  styleUrls: ['./revenue-line-chart.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class RevenueLineChartComponent
  implements OnChanges, AfterViewInit, OnDestroy {

  @Input() labels: string[] = [];
  @Input() values: number[] = [];

  private viewReady = false;
  private pendingRender = false;

  chart?: ApexCharts;
  themeObserver?: MutationObserver;
  private readonly onResize = () => this.safeRender();

  constructor(private dateUtils: DateUtilsService) { }
  
  ngAfterViewInit(): void {
    this.viewReady = true;
    this.watchResize();

    if (this.pendingRender) {
      this.pendingRender = false;
      this.safeRender();
    }
  }

  ngOnInit(): void {
    this.watchTheme();
  }

  ngOnChanges(): void {
    if (!this.labels.length || !this.values.length) return;

    if (!this.viewReady) {
      this.pendingRender = true;
      return;
    }

    this.safeRender();
  }

  ngOnDestroy(): void {
    this.chart?.destroy();
    this.themeObserver?.disconnect();
    window.removeEventListener('resize', this.onResize);
  }

  /* =========================
     THEME WATCHER
  ========================= */

  private watchTheme(): void {
    if (this.themeObserver) return;

    this.themeObserver = observeThemeChange(() => {
      if (!this.viewReady) return;

      // Let Angular + CSS finish first
      setTimeout(() => {
        this.chart?.destroy();
        this.chart = undefined;
        this.safeRender();
      });
    });
  }

  /* =========================
     RENDER
  ========================= */

private formatDate(d: string): string {
  return this.dateUtils.formatShort(d);
}

  private createChart(): void {
  const isMobile = window.matchMedia('(max-width: 640px)').matches;
  const isDark = document.body.classList.contains('dark');

  this.chart = new ApexCharts(
    document.querySelector('#revenue-line-chart')!,
    this.buildOptions(isMobile, isDark)
  );

  this.chart.render();
}

  private updateChart(): void {
  const isMobile = window.matchMedia('(max-width: 640px)').matches;
  const isDark = document.body.classList.contains('dark');

  this.chart!.updateOptions(
    this.buildOptions(isMobile, isDark),
    false,
    true
  );
}

  private buildOptions(isMobile: boolean, isDark: boolean): ApexCharts.ApexOptions {
  return {
    chart: {
      type: 'area',
      width: Math.max(this.labels.length * 80, 900),
      height: isMobile ? 300 : 320,
      toolbar: { show: false },
      zoom: { enabled: false },
      foreColor: isDark ? '#f1f5f9' : '#111827'
    },

    series: [{ name: 'Revenue', data: this.values }],

    xaxis: {
      categories: this.labels.map(d => this.formatDate(d)),
      tickPlacement: 'between',
      labels: {
        rotate: -30,
        offsetY: 6,
        style: {
          fontSize: '11px',
          colors: isDark ? '#cbd5e1' : '#4b5563'
        }
      },
      axisBorder: { show: false },
      axisTicks: { show: false }
    },

    yaxis: {
      labels: {
        formatter: v => `KSh ${v.toLocaleString('en-KE')}`,
        style: {
          fontSize: '11px',
          colors: isDark ? '#cbd5e1' : '#4b5563'
        }
      }
    },

    grid: {
      padding: {
        left: 12,
        right: 12,
        top: 8,
        bottom: isMobile ? 58 : 40
      },
      borderColor: isDark ? '#475569' : '#d1d5db',
      strokeDashArray: 3
    },

    stroke: { curve: 'smooth', width: 2.5 },

    fill: {
      type: 'gradient',
      gradient: {
        shadeIntensity: 0.15,
        opacityFrom: 0.28,
        opacityTo: 0.02
      }
    },

    tooltip: {
      theme: isDark ? 'dark' : 'light',
      y: {
        formatter: v => `KSh ${v.toLocaleString('en-KE')}`
      }
    },

    colors: ['#3b82f6']
  };
}

  private safeRender(): void {
  requestAnimationFrame(() => {
  if (!this.viewReady) return;

  if (this.chart) {
    this.updateChart();
  } else {
    this.createChart();
  }
});
  }

  private watchResize(): void {
  window.addEventListener('resize', this.onResize);
}
}