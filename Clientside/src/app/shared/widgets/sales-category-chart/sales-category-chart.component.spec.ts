import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SalesCategoryChartComponent } from './sales-category-chart.component';

describe('SalesCategoryChartComponent', () => {
  let component: SalesCategoryChartComponent;
  let fixture: ComponentFixture<SalesCategoryChartComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SalesCategoryChartComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SalesCategoryChartComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
