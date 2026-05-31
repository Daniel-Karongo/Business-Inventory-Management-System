export const environment = {
  production: true,

  idleLogoutMinutes: 60,

  apiUrl: `/api`,

  endpoints: {
    /* ============================================================
       AUTH
    ============================================================ */
    auth: {
      login: '/auth/login',
      bulkLogin: '/auth/login/bulk',
      logout: '/auth/logout',

      passwordReset: {
        options: '/auth/password-reset/options',
        initiate: '/auth/password-reset/initiate',
        complete: '/auth/password-reset/complete',
        verify: '/auth/password-reset/verify',
        force: '/auth/password-reset/force'
      }
    },

    /* ============================================================
       USERS
    ============================================================ */
    users: {
      base: '/users',

      /* ---------- CREATE / UPDATE ---------- */
      register: '/users/register',  // POST (multipart)
      registerBulk: '/users/register/bulk',

      update: (identifier: string) => `/users/${identifier}`, // PATCH
      updateImages: (identifier: string) => `/users/${identifier}/images`,

      /* ---------- GET ---------- */
      get: (identifier: string, deleted: boolean = false) =>
        `/users/user/${identifier}?deleted=${deleted}`,

      getAll: (deleted?: boolean) =>
        deleted === undefined
          ? `/users/all`
          : `/users/all?deleted=${deleted}`,

      byRole: (role: string, deleted: boolean = false) =>
        `/users/role/${role}/active?deleted=${deleted}`,

      /* ---------- DELETE / RESTORE ---------- */
      softDelete: (id: string) => `/users/soft/${id}`,
      softDeleteBulk: '/users/soft/bulk',

      restore: (id: string) => `/users/restore/${id}`,
      restoreBulk: '/users/restore/bulk',

      hardDelete: (id: string) => `/users/hard/${id}`,
      hardDeleteBulk: '/users/hard/bulk',

      /* ---------- IMAGES ---------- */
      images: {
        all: (deletedImages?: boolean, deletedUsers?: boolean) =>
          `/users/images/all?deletedImages=${deletedImages}&deletedUsers=${deletedUsers}`,

        allDownload: (deletedImages?: boolean, deletedUsers?: boolean) =>
          `/users/images/all/download?deletedImages=${deletedImages}&deletedUsers=${deletedUsers}`,

        forUser: (identifier: string) =>
          `/users/images/all/${identifier}`,

        forUserDownload: (identifier: string) =>
          `/users/images/all/download/${identifier}`,

        one: (identifier: string, filename: string, deleted?: boolean) =>
          `/users/images/${identifier}/${filename}?deleted=${deleted}`,

        softDelete: (identifier: string, filename: string) =>
          `/users/images/${identifier}/${filename}/soft`,

        softDeleteAll: (identifier: string) =>
          `/users/images/all/${identifier}/soft`,

        restore: (identifier: string, filename: string) =>
          `/users/images/${identifier}/${filename}/restore`,

        restoreAll: (identifier: string) =>
          `/users/images/all/${identifier}/restore`,

        hardDelete: (identifier: string, filename: string) =>
          `/users/images/${identifier}/${filename}/hard`,

        hardDeleteAll: (identifier: string) =>
          `/users/images/all/${identifier}/hard`,
      },

      /* ---------- AUDITS ---------- */
      audits: {
        userTarget: (identifier: string) =>
          `/users/audits/${identifier}/target`,

        userDoer: (identifier: string) =>
          `/users/audits/${identifier}/doer`,

        imageTarget: (identifier: string) =>
          `/users/images/audits/${identifier}/receiver`,

        imageDoer: (identifier: string) =>
          `/users/images/audits/${identifier}/doer`,
      },

      roles: {
        base: '/roles',
        get: (id: string) => `/roles/${id}`,
        create: '/roles',
        update: (id: string) => `/roles/${id}`,
      }
    },

    /* ============================================================
       BRANCHES
    ============================================================ */
    branches: {
      base: '/branches',

      create: '/branches',
      bulkCreate: '/branches/bulk',

      getAll: (deleted?: boolean) => `/branches?deleted=${deleted}`,
      getById: (id: string) => `/branches/${id}`,

      update: (id: string) => `/branches/${id}`,

      delete: (id: string, soft: boolean) => `/branches/${id}?soft=${soft}`,
      deleteBulk: (soft: boolean) => `/branches/bulk?soft=${soft}`,

      restore: (id: string) => `/branches/restore/${id}`,
      restoreBulk: '/branches/restore/bulk',
    },

    /* ============================================================
       DEPARTMENTS
    ============================================================ */
    departments: {
      base: '/departments',

      create: '/departments',
      bulkCreate: '/departments/bulk',

      update: (id: string) => `/departments/${id}`,

      get: (id: string) => `/departments/${id}`,
      getAll: (deleted?: boolean) => `/departments?deleted=${deleted}`,

      getForUser: (userId: string) => `/departments/user/${userId}`,

      audits: {
        forDepartment: (id: string) => `/departments/${id}/audits`,
        all: '/departments/all/audits',
        performedBy: (userId: string) =>
          `/departments/audits/performer/${userId}`
      },

      delete: (id: string, soft: boolean = true) =>
        `/departments/delete/${id}?soft=${soft}`,

      deleteBulk: (soft: boolean = true) =>
        `/departments/delete/bulk?soft=${soft}`,

      restore: (id: string) => `/departments/restore/${id}`,
      restoreBulk: '/departments/restore/bulk'
    },

    /* ============================================================
       CATEGORIES
    ============================================================ */
    categories: {
      base: '/categories',

      create: '/categories',
      createBulk: '/categories/bulk',

      updateRecursive: (id: number) =>
        `/categories/${id}/recursive`,

      all: (mode = 'tree', deleted = false) =>
        `/categories/all?mode=${mode}&deleted=${deleted}`,

      search: (keyword: string, deleted = false) =>
        `/categories/search?keyword=${keyword}&deleted=${deleted}`,

      get: (id: number, mode = 'tree', deleted = false) =>
        `/categories/${id}?mode=${mode}&deleted=${deleted}`,

      suppliers: (id: number, deleted = false, strict = true) =>
        `/categories/${id}/suppliers?deleted=${deleted}&strict=${strict}`,

      softDelete: (id: number) => `/categories/${id}/soft`,
      softDeleteBulk: '/categories/bulk/soft',

      hardDelete: (id: number) => `/categories/${id}/hard`,
      hardDeleteBulk: '/categories/bulk/hard',

      restore: (id: number) => `/categories/${id}/restore`,
      restoreBulk: '/categories/restore/bulk',

      restoreRecursive: (id: number) =>
        `/categories/${id}/restore-recursive`,
      restoreRecursiveBulk: '/categories/restore-recursive/bulk'
    },

    /* ============================================================
       PAYMENTS
    ============================================================ */
    payments: {

      base: '/payments',

      create: (branchId: string) =>
        `/payments/branch/${branchId}`,

      get: (
        branchId: string,
        id: string
      ) =>
        `/payments/branch/${branchId}/${id}`,

      list: (branchId: string) =>
        `/payments/branch/${branchId}`,

      refund: (
        branchId: string,
        id: string
      ) =>
        `/payments/branch/${branchId}/${id}/refund`,

      reverse: (
        branchId: string,
        id: string
      ) =>
        `/payments/branch/${branchId}/${id}/reverse`,

      reconcile: (branchId: string) =>
        `/payments/branch/${branchId}/reconcile`,

      mpesa: {

        initiateStk: (
          branchId: string
        ) =>
          `/payments/mpesa/branch/${branchId}/stk/initiate`,

        callback: (
          branchId: string
        ) =>
          `/payments/mpesa/branch/${branchId}/stk/callback`,

        c2bConfirm: (
          branchId: string
        ) =>
          `/payments/mpesa/branch/${branchId}/c2b/confirm`,

        c2bValidate: (
          branchId: string
        ) =>
          `/payments/mpesa/branch/${branchId}/c2b/validate`
      }
    },
    /* =====================================================
           PRODUCTS
        ===================================================== */
    products: {
      base: '/products',
      list: '/products',
      get: (id: string) =>
        `/products/${id}`,
      getBySku: (sku: string) =>
        `/products/sku/${sku}`,
      byCategory: (categoryId: string | number) =>
        `/products/category/${categoryId}`,
      bySupplier: (supplierId: string) =>
        `/products/supplier/${supplierId}`,
      create: '/products',
      update: (id: string) =>
        `/products/${id}`,
      softDelete: (id: string) =>
        `/products/${id}/soft`,
      hardDelete: (id: string) =>
        `/products/${id}/hard`,
      restore: (id: string) =>
        `/products/${id}/restore`,
      audits: (id: string) =>
        `/products/${id}/audits`,
      thumbnail: (id: string) =>
        `/products/images/${id}/thumbnail`,
      /* =========================================
         SEARCH
      ========================================= */
      search: {
        base: '/products/search'
      },
      /* =========================================
         BULK
      ========================================= */
      bulk: {
        fullCreate: '/products/bulk/full',
        softDelete: '/products/bulk',
        restore: '/products/bulk/restore',
        hardDelete: '/products/bulk/hard'
      },
      /* =========================================
         IMAGES
      ========================================= */
      images: {
        base: '/products/images',
        list: (productId: string) =>
          `/products/images/${productId}`,
        upload: (productId: string) =>
          `/products/images/${productId}`,
        deleteOne: (
          productId: string,
          filename: string
        ) =>
          `/products/images/${productId}/${filename}`,
        deleteAll: (productId: string) =>
          `/products/images/${productId}`,
        restore: (
          productId: string,
          imageId: string
        ) =>
          `/products/images/${productId}/${imageId}/restore`,
        zip: (productId: string) =>
          `/products/images/${productId}/zip`,
        all: '/products/images',
        allZip: '/products/images/zip',
        auditHistory: (
          productId: string
        ) =>
          `/products/images/${productId}/audits`,
      },
      /* =========================================
         VARIANTS
      ========================================= */
      variants: {
        create: '/product-variants',

        get: (id: string) =>
          `/product-variants/${id}`,
        forProduct: (productId: string) =>
          `/product-variants/product/${productId}`,
        update: (id: string) =>
          `/product-variants/${id}`,
        delete: (id: string) =>
          `/product-variants/${id}`,
        restore: (id: string) =>
          `/product-variants/${id}/restore`,
        hardDelete: (id: string) =>
          `/product-variants/${id}/hard`,
        audits: (id: string) =>
          `/product-variants/${id}/audits`,
        images: {
          list: (variantId: string) =>
            `/product-variants/${variantId}/images`,
          all: (id: string) =>
            `/product-variants/${id}/images/all`,
          upload: (variantId: string) =>
            `/product-variants/${variantId}/images`,
          image: (
            variantId: string,
            fileName: string
          ) =>
            `/product-variants/${variantId}/images/${fileName}`,
          thumbnail: (
            variantId: string,
            fileName: string
          ) =>
            `/product-variants/${variantId}/thumbnails/${fileName}`,
          zip: (variantId: string) =>
            `/product-variants/${variantId}/images/zip`,
          auditHistory: (variantId: string) =>
            `/product-variants/${variantId}/image-audits`,
        },
        barcodePdf: {
          bulk: '/product-variants/barcode/pdf/bulk',
          product: (productId: string) =>
            `/product-variants/product/${productId}/barcode/pdf`,
          download: (fileName: string) =>
            `/product-variants/barcode/pdf/download/${fileName}`
        },
        packaging: {
          base: '/product-variants/packaging',
          create: '/product-variants/packaging',
          get: (variantId: string) =>
            `/product-variants/packaging/${variantId}`,
          basePackaging: (variantId: string) =>
            `/product-variants/packaging/${variantId}/base`,
          update: (id: string) =>
            `/product-variants/packaging/${id}`,
          delete: (id: string) =>
            `/product-variants/packaging/${id}`
        },
        pricing: {
          base: '/product-variants/pricing',
          forVariant: (variantId: string) =>
            `/product-variants/pricing/variant/${variantId}`,
          create: '/product-variants/pricing',
          update: (id: string) =>
            `/product-variants/pricing/${id}`,
          delete: (id: string) =>
            `/product-variants/pricing/${id}`
        }
      }
    },
    /* =====================================================
       BARCODES
    ===================================================== */
    barcodes: {
      base: '/barcodes',
      scan: '/barcodes/scan',
      lookup: (barcode: string) =>
        `/barcodes/${barcode}`,
      generate: (variantId: string) =>
        `/barcodes/variant/${variantId}`,
      image: (variantId: string) =>
        `/barcodes/variant/${variantId}/image`
    },
    /* =====================================================
       SELLABLE
    ===================================================== */
    sellable: {
      base: '/sellable',
      resolve: '/sellable/resolve',
      search: '/sellable/search'
    },

    /* =====================================================
       SALES
    ===================================================== */
    sales: {
      base: '/sales',
      create: '/sales',
      list: '/sales',
      get: (id: string) =>
        `/sales/${id}`,
      update: (id: string) =>
        `/sales/${id}`,
      deliver: (id: string) =>
        `/sales/${id}/deliver`,
      cancel: (id: string) =>
        `/sales/${id}/cancel`,
      refund: (id: string) =>
        `/sales/${id}/refund`,
      cancelAndRefund: (id: string) =>
        `/sales/${id}/cancel-refund`,
      payments: (id: string) =>
        `/sales/${id}/payments`,
      receipt: (id: string) =>
        `/sales/${id}/receipt`,
      previewLine: '/sales/preview-line',
      bulk: {
        create: '/sales/bulk'
      }
    },

    /* =====================================================
       STOCK / INVENTORY
    ===================================================== */

    stock: {
      /* =========================================
         INVENTORY QUERY
      ========================================= */
      inventory: {
        base: '/stock/inventory',
        list: '/stock/inventory',
        byBranch: (branchId: string) =>
          `/stock/inventory/branch/${branchId}`,
        variantAll: (variantId: string) =>
          `/stock/inventory/variant/${variantId}`,
        variantBranch: (
          variantId: string,
          branchId: string
        ) =>
          `/stock/inventory/variant/${variantId}/branch/${branchId}`,
        productAcrossBranches: (productId: string) =>
          `/stock/inventory/product/${productId}/branches`,
        productInBranch: (
          productId: string,
          branchId: string
        ) =>
          `/stock/inventory/product/${productId}/branch/${branchId}`
      },
      /* =========================================
         OPERATIONS
      ========================================= */
      operations: {
        base: '/stock/operations',
        receive: '/stock/operations/receive',
        transfer: '/stock/operations/transfer',
        adjust: '/stock/operations/adjust',
        consume: '/stock/operations/consume'
      },
      /* =========================================
         BULK
      ========================================= */
      bulk: {
        receive: '/stock/bulk/receive'
      },
      /* =========================================
         RESERVATIONS
      ========================================= */
      reservations: {
        base: '/stock/reservations',
        reserve: '/stock/reservations/reserve',
        release: '/stock/reservations/release',
        preview: '/stock/reservations/preview'
      },
      /* =========================================
         BATCHES
      ========================================= */
      batches: {
        base: '/stock/batches',
        variantBranch: (
          variantId: string,
          branchId: string
        ) =>
          `/stock/batches/variant/${variantId}/branch/${branchId}`,
        consumptions: (batchId: string) =>
          `/stock/batches/${batchId}/consumptions`,
        suggest: '/stock/batches/suggest'
      },
      /* =========================================
         TRANSACTIONS
      ========================================= */
      transactions: {
        base: '/stock/transactions',
        all: (branchId: string) =>
          `/stock/transactions/branch/${branchId}`,
        byVariant: (
          branchId: string,
          variantId: string
        ) =>
          `/stock/transactions/branch/${branchId}/variant/${variantId}`,
        byProduct: (
          branchId: string,
          productId: string
        ) =>
          `/stock/transactions/branch/${branchId}/product/${productId}`
      },
      /* =========================================
         REPORTS
      ========================================= */
      reports: {
        base: '/stock/reports',
        lowStock: '/stock/reports/low-stock',
        outOfStock: '/stock/reports/out-of-stock',
        audit: (productId: string) =>
          `/stock/reports/audit/${productId}`
      },
      /* =========================================
         VALUATION
      ========================================= */
      valuation: {
        base: '/stock/valuation',
        total: '/stock/valuation',
        dashboard: '/stock/valuation/dashboard',
        product: (productId: string) =>
          `/stock/valuation/product/${productId}`,
        branch: (branchId: string) =>
          `/stock/valuation/branch/${branchId}`,
        categories: '/stock/valuation/categories',
        history: '/stock/valuation/history',
        historyVariant: (variantId: string) =>
          `/stock/valuation/history/${variantId}`
      },
      /* =========================================
         ONBOARDING
      ========================================= */
      onboarding: {
        base: '/stock/onboarding',
        create: '/stock/onboarding',
        bulk: '/stock/onboarding/bulk'
      }
    },

    /* ============================================================
       SUPPLIERS
    ============================================================ */
    suppliers: {
      base: '/suppliers',

      register: '/suppliers/register',
      registerBulk: '/suppliers/register/bulk',

      update: (id: string) => `/suppliers/${id}`,
      updateImages: (id: string) => `/suppliers/${id}/images`,

      getAll: (deleted?: boolean) => `/suppliers/all?deleted=${deleted}`,
      getByIdentifier: (identifier: string, deleted?: boolean) =>
        `/suppliers/identifier/${identifier}?deleted=${deleted}`,

      advancedSearch: '/suppliers/advanced',

      getImages: (id: string, deleted?: boolean) =>
        `/suppliers/${id}/images?deleted=${deleted}`,

      downloadImage: (id: string, filename: string, deleted?: boolean) =>
        `/suppliers/${id}/images/${filename}?deleted=${deleted}`,

      downloadImagesZip: (id: string, deleted?: boolean) =>
        `/suppliers/${id}/images/zip?deleted=${deleted}`,

      allImages: (deletedSupplier?: boolean, deletedImage?: boolean) =>
        `/suppliers/images/all?deletedSupplier=${deletedSupplier}&deletedImage=${deletedImage}`,

      downloadAllImages: (deletedSupplier?: boolean, deletedImage?: boolean) =>
        `/suppliers/images/all/download?deletedSupplier=${deletedSupplier}&deletedImage=${deletedImage}`,

      softDelete: (id: string) => `/suppliers/${id}/soft`,
      softDeleteBulk: '/suppliers/bulk/soft',

      restore: (id: string) => `/suppliers/restore/${id}`,
      restoreBulk: '/suppliers/restore/bulk',

      hardDelete: (id: string) => `/suppliers/${id}/hard`,
      hardDeleteBulk: '/suppliers/bulk/hard',

      audits: {
        imageAudit: (identifier: string) =>
          `/suppliers/images/audit/${identifier}`,
        imageAuditAll: '/suppliers/all/images/audit',

        supplierAudit: (identifier: string) =>
          `/suppliers/${identifier}/audit`,
        supplierAuditAll: '/suppliers/all/audit',
      }
    },

    /* ============================================================
       CUSTOMERS
    ============================================================ */
    customers: {
      base: '/customers',

      create: '/customers',
      list: '/customers',
      search: '/customers/search',

      get: (id: string) => `/customers/${id}`,

      update: (id: string) => `/customers/${id}`,
      delete: (id: string) => `/customers/${id}`,

      payments: (id: string) => `/customers/${id}/payments`,
      sales: (id: string) => `/customers/${id}/sales`,
    },

    /* ============================================================
       ACCOUNTS (You will add when controller provided)
    ============================================================ */
    accounts: {
      base: '/accounts'
    },
    manualJournals: {
      base: '/accounting/manual-journals'
    },

    /* ============================================================
       DEVICES AND BIOMETRICS
    ============================================================ */

    security: {

      devices: {
        list: (branchId: string | null) =>
          branchId
            ? `/admin/devices/branch/${branchId}`
            : `/admin/devices/branch/null`,

        pending: `/admin/devices/pending`,

        approve: (id: string) => `/admin/devices/${id}/approve`,
        reject: (id: string) => `/admin/devices/${id}/reject`,
        rename: (id: string) => `/admin/devices/${id}/rename`,
        stats: `/admin/devices/stats`,
        audit: (id: string) => `/admin/devices/${id}/audit`
      },

      platformDevices: {
        list: '/platform/devices',
        approve: (id: string) => `/platform/devices/${id}/approve`,
        reject: (id: string) => `/platform/devices/${id}/reject`,
        rename: (id: string) => `/platform/devices/${id}/rename`,
        audit: (id: string) => `/platform/devices/${id}/audit`
      },

      biometrics: {
        mine: '/biometrics',
        rename: (id: string) => `/biometrics/${id}/rename`,
        delete: (id: string) => `/biometrics/${id}`,
        adminUser: (userId: string) => `/admin/biometrics/user/${userId}`,
        stats: '/admin/biometrics/stats',
        adminDelete: (id: string) => `/admin/biometrics/${id}`
      }

    }
  }
};