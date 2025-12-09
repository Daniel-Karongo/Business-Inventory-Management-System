export const environment = {
  production: false,
  apiUrl: 'http://localhost:8080/api',

  endpoints: {
    /* ======================== AUTH ======================== */
    auth: {
      login: '/auth/login',
      bulkLogin: '/auth/login/bulk',
      logout: '/auth/logout'
    },

    /* ======================== USERS ======================== */
    users: {
      base: '/users',

      register: '/users/register',
      registerBulk: '/users/register/bulk',

      update: (identifier: string) => `/users/${identifier}`,
      updateImages: (identifier: string) => `/users/${identifier}/images`,

      getUser: (identifier: string) => `/users/user/${identifier}`,
      getAll: '/users/all',

      byRole: (role: string) => `/users/role/${role}/active`,

      softDelete: (id: string) => `/users/soft/${id}`,
      softDeleteBulk: '/users/soft/bulk',

      restore: (id: string) => `/users/restore/${id}`,
      restoreBulk: '/users/restore/bulk',

      hardDelete: (id: string) => `/users/hard/${id}`,
      hardDeleteBulk: '/users/hard/bulk',

      /* IMAGES */
      images: {
        all: '/users/images/all',
        allDownload: '/users/images/all/download',

        forUser: (identifier: string) => `/users/images/all/${identifier}`,
        forUserDownload: (identifier: string) => `/users/images/all/download/${identifier}`,

        one: (identifier: string, filename: string) =>
          `/users/images/${identifier}/${filename}`,

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

      /* AUDITS */
      audits: {
        userTarget: (identifier: string) => `/users/audits/${identifier}/target`,
        userDoer: (identifier: string) => `/users/audits/${identifier}/doer`,

        imageTarget: (identifier: string) =>
          `/users/images/audits/${identifier}/receiver`,
        imageDoer: (identifier: string) =>
          `/users/images/audits/${identifier}/doer`,
      }
    },

    /* ======================== BRANCHES ======================== */
    branches: {
      base: '/branches',

      create: '/branches',
      bulkCreate: '/branches/bulk',

      getById: (id: string) => `/branches/${id}`,
      getAll: '/branches',

      update: (id: string) => `/branches/${id}`,

      delete: (id: string) => `/branches/${id}`,
      deleteBulk: '/branches/bulk',

      restore: (id: string) => `/branches/restore/${id}`,
      restoreBulk: '/branches/restore/bulk'
    },

    /* ======================== DEPARTMENTS ======================== */
    departments: {
      base: '/departments',

      create: '/departments',
      bulkCreate: '/departments/bulk',

      update: (id: string) => `/departments/${id}`,

      getById: (id: string) => `/departments/${id}`,
      getAll: '/departments',

      getForUser: (userId: string) => `/departments/user/${userId}`,

      audits: {
        forDepartment: (id: string) => `/departments/${id}/audits`,
        all: '/departments/all/audits',
        performer: (id: string) => `/departments/audits/performer/${id}`,
      },

      delete: (id: string, soft = true) =>
        `/departments/delete/${id}?soft=${soft}`,

      deleteBulk: (soft = true) =>
        `/departments/delete/bulk?soft=${soft}`,

      restore: (id: string) => `/departments/restore/${id}`,
      restoreBulk: '/departments/restore/bulk'
    },

    categories: {
      base: '/categories',
      search: '/categories/search',
      flat: '/categories/all/flat',
      suppliers: '/categories/{id}/suppliers'
    },

    suppliers: {
      base: '/suppliers',
      register: '/suppliers/register',
      bulk: '/suppliers/register/bulk',
      images: '/suppliers/{id}/images'
    },

    products: {
      base: '/products',
      variants: '/variants',
      barcode: '/products/barcode'
    },

    inventory: {
      base: '/inventory'
    },

    customers: {
      base: '/customers',
      search: '/customers/search'
    },

    sales: {
      base: '/sales',
      cancel: '/sales/{id}/cancel',
      refund: '/sales/{id}/refund'
    },

    payments: {
      base: '/payments',
      refund: '/payments/{id}/refund',
      reverse: '/payments/{id}/reverse',
      reconcile: '/payments/reconcile',
      mpesa: {
        initiateStk: '/payments/mpesa/stk/initiate'
      }
    },

    accounts: {
      base: '/accounts'
    }
  }
};