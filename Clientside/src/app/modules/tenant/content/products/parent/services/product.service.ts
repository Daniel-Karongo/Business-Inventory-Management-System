import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs';
import { environment } from '../../../../../../../environments/environment';
import { ApiResponse } from '../../../../../../core/models/api-response.model';
import { PageWrapper } from '../../../../../../core/models/page-wrapper.model';
import { BulkResult } from '../../../../../../shared/models/bulk-import.model';
import { Product, ProductCreateDTO, ProductUpdateDTO } from '../../../stock/models/product.model';

@Injectable({
    providedIn: 'root'
})
export class ProductService {
    private api = environment.apiUrl;

    private endpoints =
        environment.endpoints.products;

    constructor(
        private http: HttpClient
    ) { }

    getAll(
        branchId: string,
        deleted?: boolean
    ) {
        let params = new HttpParams()
            .set('branchId', branchId);

        if (deleted !== undefined) {
            params = params.set(
                'deleted',
                deleted
            );
        }

        return this.http
            .get<ApiResponse<Product[]>>(
                this.api + this.endpoints.list,
                { params }
            )
            .pipe(
                map(res => res.data ?? [])
            );
    }

    search(params: {
        branchId?: string;
        categoryIds?: number[];
        categoryId?: number;
        name?: string;
        description?: string;
        keyword?: string;
        minPrice?: number;
        maxPrice?: number;
        deleted?: boolean;
        page?: number;
        size?: number;
        sortBy?: string;
        direction?: 'asc' | 'desc';
        includeDeleted?: boolean;
        minSuppliers?: number;
        maxSuppliers?: number;
        supplierId?: string;
    }) {
        let httpParams = new HttpParams();

        Object.entries(params)
            .forEach(([key, value]) => {
                if (
                    value !== undefined &&
                    value !== null
                ) {
                    if (Array.isArray(value)) {
                        value.forEach(v => {
                            httpParams = httpParams.append(key, v);
                        });
                    } else {
                        httpParams = httpParams.set(
                            key,
                            String(value)
                        );
                    }
                }
            });

        return this.http.get<
            PageWrapper<Product>
        >(
            this.api +
            this.endpoints.search.base,
            {
                params: httpParams
            }
        );
    }

    getById(
        id: string,
        branchId: string,
        deleted?: boolean
    ) {
        let params = new HttpParams()
            .set('branchId', branchId);

        if (deleted !== undefined) {
            params = params.set(
                'deleted',
                deleted
            );
        }

        return this.http
            .get<ApiResponse<Product>>(
                this.api +
                this.endpoints.get(id),
                { params }
            )
            .pipe(
                map(res => res.data as Product)
            );
    }

    create(
        branchId: string,
        payload: ProductCreateDTO
    ) {
        return this.http.post<ApiResponse<Product>>(
            this.api +
            this.endpoints.create,
            payload,
            {
                params: new HttpParams()
                    .set('branchId', branchId)
            }
        );
    }

    update(
        id: string,
        payload: ProductUpdateDTO
    ) {
        return this.http.put<ApiResponse<Product>>(
            this.api +
            this.endpoints.update(id),
            payload
        );
    }

    softDelete(
        id: string,
        reason?: string
    ) {
        let params = new HttpParams();

        if (reason) {
            params = params.set(
                'reason',
                reason
            );
        }

        return this.http.delete<void>(
            this.api +
            this.endpoints.softDelete(id),
            { params }
        );
    }

    restore(
        id: string,
        restoreOptions?: {
            restoreInventory?: boolean;
            restoreStockTransactions?: boolean;
        },
        reason?: string
    ) {
        let params = new HttpParams();

        if (reason) {
            params = params.set(
                'reason',
                reason
            );
        }

        return this.http.post<void>(
            this.api +
            this.endpoints.restore(id),
            restoreOptions ?? {},
            { params }
        );
    }

    hardDelete(
        id: string,
        branchId: string,
        reason?: string
    ) {
        let params = new HttpParams()
            .set('branchId', branchId);

        if (reason) {
            params = params.set(
                'reason',
                reason
            );
        }

        return this.http.delete<void>(
            this.api +
            this.endpoints.hardDelete(id),
            { params }
        );
    }

    bulkFullCreate(
        formData: FormData
    ) {
        return this.http.post<
            BulkResult<Product>
        >(
            this.api +
            this.endpoints.bulk.fullCreate,
            formData
        );
    }

    bulkSoftDelete(
        ids: string[],
        reason?: string
    ) {
        return this.http.delete<void>(
            this.api +
            this.endpoints.bulk.softDelete,
            {
                body: {
                    ids,
                    reason
                }
            }
        );
    }

    bulkRestore(
        ids: string[],
        reason?: string,
        restoreOptions?: {
            restoreInventory?: boolean;
            restoreStockTransactions?: boolean;
        }
    ) {
        return this.http.put<void>(
            this.api +
            this.endpoints.bulk.restore,
            {
                ids,
                reason,
                restoreOptions
            }
        );
    }

    bulkHardDelete(
        ids: string[],
        branchId: string,
        reason?: string
    ) {
        return this.http.delete<void>(
            this.api +
            this.endpoints.bulk.hardDelete,
            {
                params: new HttpParams()
                    .set('branchId', branchId),
                body: {
                    ids,
                    reason
                }
            }
        );
    }

    getImages(
        productId: string,
        branchId?: string,
        deleted?: boolean
    ) {
        let params = new HttpParams();

        if (branchId) {
            params = params.set(
                'branchId',
                branchId
            );
        }

        if (deleted !== undefined) {
            params = params.set(
                'deleted',
                deleted
            );
        }

        return this.http.get<string[]>(
            this.api +
            this.endpoints.images.list(productId),
            { params }
        );
    }

    uploadImages(
        productId: string,
        files: File[],
        branchId?: string
    ) {
        const formData = new FormData();

        files.forEach(file => {
            formData.append('files', file);
        });

        let params = new HttpParams();

        if (branchId) {
            params = params.set(
                'branchId',
                branchId
            );
        }

        return this.http.patch<void>(
            this.api +
            this.endpoints.images.upload(productId),
            formData,
            { params }
        );
    }

    getImageBlob(
        productId: string,
        fileName: string
    ) {
        return this.http.get(
            this.api +
            this.endpoints.images.list(productId) +
            '/' +
            fileName,
            {
                responseType: 'blob'
            }
        );
    }

    downloadImagesZip(
        productId: string,
        branchId?: string,
        deleted?: boolean
    ) {
        let params = new HttpParams();

        if (branchId) {
            params = params.set(
                'branchId',
                branchId
            );
        }

        if (deleted !== undefined) {
            params = params.set(
                'deleted',
                deleted
            );
        }

        return this.http.get(
            this.api +
            this.endpoints.images.zip(productId),
            {
                params,
                responseType: 'blob'
            }
        );
    }

    getAudits(
        productId: string,
        branchId: string
    ) {
        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.audits(productId),
                {
                    params: new HttpParams()
                        .set('branchId', branchId)
                }
            )
            .pipe(
                map(res => res.data ?? [])
            );
    }
}