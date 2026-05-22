import {
    Component,
    EventEmitter,
    Input,
    Output
} from '@angular/core';

import { CommonModule }
    from '@angular/common';

import {
    FormsModule
} from '@angular/forms';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatIconModule
} from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
    selector: 'app-enterprise-paginator',
    standalone: true,
    imports: [
        CommonModule,
        FormsModule,
        MatButtonModule,
        MatSelectModule,
        MatFormFieldModule,
        MatIconModule,
        MatTooltipModule
    ],
    templateUrl:
        './enterprise-paginator.component.html',
    styleUrls: [
        './enterprise-paginator.component.scss'
    ]
})
export class EnterprisePaginatorComponent {

    @Input()
    page = 0;

    @Input()
    size = 25;

    @Input()
    totalPages = 0;

    @Input()
    totalElements = 0;

    @Input()
    loading = false;

    @Output()
    pageChange =
        new EventEmitter<number>();

    @Output()
    sizeChange =
        new EventEmitter<number>();

    readonly pageSizes = [
        5,
        10,
        15,
        20,
        25,
        50,
        100,
        200,
        500,
        1000
    ];

    first(): void {

        if (
            this.isFirstPage
            || this.loading
        ) {
            return;
        }

        this.pageChange.emit(0);
    }

    previous(): void {

        if (
            this.isFirstPage
            || this.loading
        ) {
            return;
        }

        this.pageChange.emit(
            this.page - 1
        );
    }

    next(): void {

        if (
            this.isLastPage
            || this.loading
        ) {
            return;
        }

        this.pageChange.emit(
            this.page + 1
        );
    }

    last(): void {

        if (
            this.isLastPage
            || this.loading
        ) {
            return;
        }

        this.pageChange.emit(
            this.totalPages - 1
        );
    }

    updateSize(
        value: number
    ): void {

        if (
            value === this.size
        ) {
            return;
        }

        this.sizeChange.emit(value);
    }

    get start(): number {

        if (
            !this.totalElements
            || !this.size
        ) {
            return 0;
        }

        return (
            this.page * this.size
        ) + 1;
    }

    get end(): number {

        if (
            !this.totalElements
            || !this.size
        ) {
            return 0;
        }

        return Math.min(
            (
                this.page + 1
            ) * this.size,
            this.totalElements
        );
    }

    get displayPage(): number {

        if (!this.totalPages) {
            return 0;
        }

        return this.page + 1;
    }

    get isFirstPage(): boolean {

        return (
            !this.totalPages
            || this.page <= 0
        );
    }

    get isLastPage(): boolean {

        return (
            !this.totalPages
            || this.page >=
            this.totalPages - 1
        );
    }

}