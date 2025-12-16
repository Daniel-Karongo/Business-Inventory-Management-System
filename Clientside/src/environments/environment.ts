export const environment = {
  production: false,
  apiUrl: 'http://localhost:8080/api',

  endpoints: {
    /* ============================================================
       AUTH
    ============================================================ */
    auth: {
      login: '/auth/login',
      bulkLogin: '/auth/login/bulk',
      logout: '/auth/logout'
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

      allFlat: (mode = 'tree', deleted = false) =>
        `/categories/all/flat?mode=${mode}&deleted=${deleted}`,

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
       PRODUCTS & VARIANTS
    ============================================================ */
    products: {
      base: '/products',

      list: (deleted?: boolean) => `/products?deleted=${deleted}`,
      advanced: '/products/advanced',

      create: '/products/create',
      bulkCreate: '/products/create/bulk',

      update: (id: string) => `/products/${id}`,

      softDelete: (id: string) => `/products/soft/${id}`,
      hardDelete: (id: string) => `/products/hard/${id}`,
      restore: (id: string) => `/products/restore/${id}`,
      getById: (id: string) => `/products/${id}`,
      search: (keyword: string) => `/products/search?keyword=${keyword}`,

      variants: {
        base: '/product-variants',

        create: '/product-variants',
        forProduct: (productId: string) =>
          `/product-variants/product/${productId}`,
        get: (id: string) => `/product-variants/${id}`,
        update: (id: string) => `/product-variants/${id}`,
        delete: (id: string) => `/product-variants/${id}`,
      },

      images: {
        list: (id: string) => `/products/${id}/images`,
        upload: (id: string) => `/products/${id}/images`,
        deleteOne: (id: string, filename: string) =>
          `/products/${id}/images/${filename}`,
        deleteAll: (id: string) => `/products/${id}/images`,
        zip: (id: string) => `/products/${id}/images/zip`
      },

      barcode: {
        generateImage: '/products/barcode/image',
        generateLabel: '/products/barcode/label',
        generateSheet: '/products/barcode/sheet'
      },

      audits: (id: string) => `/products/${id}/audits`
    },

    /* ============================================================
       INVENTORY
    ============================================================ */
    inventory: {
      base: '/inventory',

      receive: '/inventory/receive',
      bulkReceive: '/inventory/receive/bulk',

      adjust: '/inventory/adjust/variant',

      decrement: '/inventory/variant/decrement',
      reserve: '/inventory/variant/reserve',
      release: '/inventory/variant/release',

      getVariantAcrossBranches: (variantId: string) =>
        `/inventory/variant/${variantId}`,

      getVariantForBranch: (variantId: string, branchId: string) =>
        `/inventory/variant/${variantId}/branch/${branchId}`,

      listForBranch: (branchId: string) =>
        `/inventory/branch/${branchId}`,

      productStockAcrossBranches: (productId: string) =>
        `/inventory/product/stock-across-branches/${productId}`,

      productStockInBranch: (productId: string, branchId: string) =>
        `/inventory/product/${productId}/branch/${branchId}`,

      lowStock: (threshold: number) =>
        `/inventory/low-stock?threshold=${threshold}`,

      outOfStock: `/inventory/out-of-stock`,

      transactions: '/inventory/transactions',
      transactionsForProduct: (productId: string) =>
        `/inventory/transactions/product/${productId}`,

      valuation: {
        dashboard: '/inventory/valuation/dashboard',
        base: '/inventory/valuation',
        product: (productId: string) =>
          `/inventory/valuation/product/${productId}`,
        branch: (branchId: string) =>
          `/inventory/valuation/branch/${branchId}`,
        history: '/inventory/valuation/history',
        byCategory: '/inventory/valuation/categories',

        snapshotTake: '/inventory/snapshot/take',
        snapshotGet: '/inventory/snapshot',
      },

      audit: (productId: string) => `/inventory/audit/${productId}`
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
       SALES
    ============================================================ */
    sales: {
      base: '/sales',

      create: '/sales',
      list: '/sales',

      get: (id: string) => `/sales/${id}`,
      update: (id: string) => `/sales/${id}`,

      cancel: (id: string) => `/sales/${id}/cancel`,
      refund: (id: string) => `/sales/${id}/refund`,
      cancelAndRefund: (id: string) => `/sales/${id}/cancel-refund`,

      payments: (id: string) => `/sales/${id}/payments`,
      receipt: (id: string) => `/sales/${id}/receipt`
    },

    /* ============================================================
       PAYMENTS
    ============================================================ */
    payments: {
      base: '/payments',

      create: '/payments',
      get: (id: string) => `/payments/${id}`,
      list: '/payments',

      refund: (id: string) => `/payments/${id}/refund`,
      reverse: (id: string) => `/payments/${id}/reverse`,

      reconcile: '/payments/reconcile',

      mpesa: {
        initiateStk: '/payments/mpesa/stk/initiate',
        callback: '/payments/mpesa/stk/callback',
        c2bConfirm: '/payments/mpesa/c2b/confirm',
        c2bValidate: '/payments/mpesa/c2b/validate',
      }
    },

    /* ============================================================
       ACCOUNTS (You will add when controller provided)
    ============================================================ */
    accounts: {
      base: '/accounts'
    }
  }
};