import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TransferStockDialogComponent } from './transfer-stock-dialog.component';

describe('TransferStockDialogComponent', () => {
  let component: TransferStockDialogComponent;
  let fixture: ComponentFixture<TransferStockDialogComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TransferStockDialogComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TransferStockDialogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
