import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserImageManagerComponent } from './user-image-manager.component';

describe('UserImageManagerComponent', () => {
  let component: UserImageManagerComponent;
  let fixture: ComponentFixture<UserImageManagerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UserImageManagerComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UserImageManagerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
