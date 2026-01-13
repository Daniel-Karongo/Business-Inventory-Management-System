import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ManualJournalComponent } from './manual-journal.component';

describe('ManualJournalComponent', () => {
  let component: ManualJournalComponent;
  let fixture: ComponentFixture<ManualJournalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ManualJournalComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ManualJournalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
