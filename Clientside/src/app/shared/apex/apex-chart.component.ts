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
  @ViewChild('chartContainer', { static: true }) chartContainer!: ElementRef<HTMLDivElement>;
  @Input() options: any; // apexcharts options object
  @Input() autoRender = true;

  private chart?: ApexCharts;

  ngAfterViewInit(): void {
    if (this.autoRender && this.options) {
      this.render();
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (!this.chart && this.autoRender && this.options && (changes['options'] || changes['autoRender'])) {
      this.render();
      return;
    }

    if (this.chart && changes['options'] && changes['options'].currentValue) {
      // update chart options
      try {
        // prefer updateOptions which updates axes/series/etc
        this.chart.updateOptions(changes['options'].currentValue, false, true);
        if (changes['options'].currentValue.series) {
          // update series too (force)
          // ApexCharts updateSeries expects array
          this.chart.updateSeries(changes['options'].currentValue.series);
        }
      } catch (e) {
        // fallback: destroy and re-render
        this.destroy();
        this.render();
      }
    }
  }

  private render() {
    if (!this.options || !this.chartContainer) return;
    this.chart = new ApexCharts(this.chartContainer.nativeElement, this.options);
    this.chart.render();
  }

  private destroy() {
    if (this.chart) {
      try {
        this.chart.destroy();
      } catch (e) {
        // ignore
      }
      this.chart = undefined;
    }
  }

  ngOnDestroy(): void {
    this.destroy();
  }
}