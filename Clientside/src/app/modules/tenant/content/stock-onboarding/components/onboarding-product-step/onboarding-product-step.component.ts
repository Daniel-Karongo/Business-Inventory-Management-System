import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnInit,
    Output,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatDialog
} from '@angular/material/dialog';

import {
    Product
} from '../../../stock/models/product.model';

import {
    BranchService
} from '../../../branches/services/branch.service';

import {
    CategoryService
} from '../../../categories/services/category.service';

import {
    BranchMinimalDTO
} from '../../../branches/models/branch.model';

import {
    Category
} from '../../../categories/models/category.model';
import { ProductSelectorDialogComponent } from '../../../sales/dialogs/product-selector-dialog/product-selector-dialog.component';

@Component({
    selector:
        'app-onboarding-product-step',

    standalone: true,

    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatIconModule
    ],

    templateUrl:
        './onboarding-product-step.component.html',

    styleUrls: [
        './onboarding-product-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingProductStepComponent
    implements OnInit {

    private readonly fb =
        inject(FormBuilder);

    private readonly dialog =
        inject(MatDialog);

    private readonly branchService =
        inject(BranchService);

    private readonly categoryService =
        inject(CategoryService);

    @Input({ required: true })
    mode!:
        | 'NEW_PRODUCT'
        | 'EXISTING_PRODUCT';

    @Output()
    modeChange =
        new EventEmitter<
            'NEW_PRODUCT'
            | 'EXISTING_PRODUCT'
        >();

    @Output()
    productSelected =
        new EventEmitter<Product>();

    @Output()
    draftChange =
        new EventEmitter<any>();

    readonly form =
        this.fb.group({

            branchId: [
                '',
                Validators.required
            ],

            name: [
                '',
                Validators.required
            ],

            description: [''],

            categoryId: [
                null as number | null,
                Validators.required
            ],

            minimumPercentageProfit: [
                null as number | null
            ]

        });

    branches:
        BranchMinimalDTO[] = [];

    categories:
        Category[] = [];

    selectedProduct:
        Product | null = null;

    constructor() {

        this.form.valueChanges
            .subscribe(value => {

                this.draftChange.emit({
                    name: value.name,
                    description:
                        value.description,
                    categoryId:
                        value.categoryId,
                    minimumPercentageProfit:
                        value.minimumPercentageProfit
                });

            });

    }
    
    ngOnInit() {

        this.loadBranches();

        this.loadCategories();

    }

    openProductSelector() {

        const ref =
            this.dialog.open(
                ProductSelectorDialogComponent,
                {
                    width: '90vw',
                    maxWidth: '1100px',
                    height: '85vh'
                }
            );

        ref.afterClosed()
            .subscribe(
                (products: Product[]) => {

                    if (
                        !products ||
                        !products.length
                    ) {
                        return;
                    }

                    this.selectedProduct =
                        products[0];

                    this.productSelected.emit(
                        products[0]
                    );

                }
            );

    }

    switchMode(
        mode:
            | 'NEW_PRODUCT'
            | 'EXISTING_PRODUCT'
    ) {

        this.modeChange.emit(mode);

    }

    private loadBranches() {

        this.branchService
            .getAll(false)
            .subscribe({
                next: branches => {

                    this.branches =
                        branches
                            .filter(b => !!b.id)
                            .map(b => ({
                                id: b.id!,
                                name: b.name,
                                branchCode:
                                    b.branchCode ?? ''
                            }));

                }
            });

    }

    private loadCategories() {

        this.categoryService
            .getAll('flat', false)
            .subscribe({
                next: categories => {

                    this.categories =
                        categories;

                }
            });

    }

}