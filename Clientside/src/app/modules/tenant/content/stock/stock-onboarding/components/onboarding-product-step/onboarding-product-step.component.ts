import {
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    EventEmitter,
    Input,
    OnChanges,
    OnInit,
    Output,
    SimpleChanges,
    computed,
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
} from '../../../models/product.model';

import {
    BranchService
} from '../../../../branches/services/branch.service';


import {
    BranchMinimalDTO
} from '../../../../branches/models/branch.model';

import { ProductSelectorDialogComponent } from '../../../../sales/dialogs/product-selector-dialog/product-selector-dialog.component';
import { CategoryService } from '../../../categories/services/category.service';
import { Category } from '../../../categories/models/category.model';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';

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
    implements OnInit, OnChanges {

    @Input()
    draft: any = null;

    @Input()
    branchId: string | null = null;

    private readonly fb =
        inject(FormBuilder);

    private readonly dialog =
        inject(MatDialog);

    private readonly branchService =
        inject(BranchService);

    private readonly categoryService =
        inject(CategoryService);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly cdr =
        inject(ChangeDetectorRef);

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
            branchId: ['', Validators.required],
            name: ['', Validators.required],
            description: [''],
            categoryId: [null as number | string | null],
            newCategoryName: [''],
            minimumPercentageProfit: [null as number | null]
        });

    branches:
        BranchMinimalDTO[] = [];

    categories:
        Category[] = [];

    selectedProduct:
        Product | null = null;

    readonly showBranchField =
        computed(() =>
            this.branches.length > 1
        );

    creatingCategory = false;

    readonly NEW_CATEGORY =
        '__NEW_CATEGORY__';


    constructor() {

        this.form.valueChanges
            .subscribe(value => {

                this.creatingCategory =
                    value.categoryId ===
                    this.NEW_CATEGORY;

                this.emitDraft();

            });

    }

    ngOnInit() {

        this.loadBranches();

        this.loadCategories();

        const currentBranch =
            this.branchContext
                .currentBranch;

        if (currentBranch) {

            this.form.patchValue({
                branchId: currentBranch
            });

        }

    }

    ngOnChanges(
        changes: SimpleChanges
    ) {

        if (
            changes['draft'] ||
            changes['branchId']
        ) {

            const categoryId =
                this.draft?.categoryId ??
                (
                    this.draft?.newCategoryName
                        ? this.NEW_CATEGORY
                        : null
                );

            this.form.patchValue({

                branchId:
                    this.branchId,

                name:
                    this.draft?.name ?? '',

                description:
                    this.draft?.description ?? '',

                categoryId,

                newCategoryName:
                    this.draft
                        ?.newCategoryName ?? '',

                minimumPercentageProfit:
                    this.draft
                        ?.minimumPercentageProfit
                    ?? null

            }, {
                emitEvent: false
            });

            this.creatingCategory =
                categoryId ===
                this.NEW_CATEGORY;

        }
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

    private emitDraft() {

        this.draftChange.emit({

            branchId:
                this.form.value.branchId,

            name:
                this.form.value.name,

            description:
                this.form.value.description,

            categoryId:
                this.form.value.categoryId,

            newCategoryName:
                this.form.value
                    .newCategoryName,

            minimumPercentageProfit:
                this.form.value
                    .minimumPercentageProfit

        });

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

                    if (
                        this.branches.length === 1
                    ) {

                        this.form.patchValue({

                            branchId:
                                this.branches[0].id

                        });

                    }

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

                    this.cdr.markForCheck();

                }

            });

    }
}