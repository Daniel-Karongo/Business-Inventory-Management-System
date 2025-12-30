import {
  Component,
  ElementRef,
  Input,
  OnChanges,
  OnDestroy,
  AfterViewInit,
  SimpleChanges,
  ViewChild
} from '@angular/core';
import { CommonModule } from '@angular/common';
import ApexCharts from 'apexcharts';

@Component({
  selector: 'app-apex-chart',
  standalone: true,
  imports: [CommonModule],
  template: `<div #chartContainer class="w-full h-full"></div>`,
  styles: [':host { display:block; height:100%; }']
})
export class ApexChartComponent implements AfterViewInit, OnChanges, OnDestroy {

  @ViewChild('chartContainer', { static: true })
  chartContainer!: ElementRef<HTMLDivElement>;

  @Input() options!: any;
  @Input() enabled = true;

  private chart?: ApexCharts;
  private initialized = false;

  ngAfterViewInit(): void {
    if (this.enabled && this.options && !this.initialized) {
      this.initChart();
    }
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (!this.enabled) return;

    if (!this.initialized && this.options) {
      this.initChart();
      return;
    }

    if (this.chart && changes['options']) {
      this.chart.updateOptions(this.options, false, true);
      if (this.options.series) {
        this.chart.updateSeries(this.options.series, true);
      }
    }
  }

  private initChart(): void {
    this.chart = new ApexCharts(
      this.chartContainer.nativeElement,
      this.options
    );
    this.chart.render();
    this.initialized = true;
  }

  ngOnDestroy(): void {
    this.chart?.destroy();
    this.chart = undefined;
  }
}