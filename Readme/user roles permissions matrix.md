# User Module

## User lifecycle

| Function           | Superuser | Admin | Manager | Supervisor | Employee | Notes                                           |
| ------------------ | --------- | ----- | ------- | ---------- | -------- | ----------------------------------------------- |
| Import users       | ✅         | ✅     | ❌       | ❌          | ❌        | `TenantAdminOnly`                               |
| Register users     | ✅         | ✅     | ❌       | ❌          | ❌        | `TenantAdminOnly`                               |
| Update user        | ✅         | ✅     | ✅       | ⚠           | ⚠         | `TenantUserOnly`, but service restricts actions |
| Update user images | ✅         | ✅     | ✅       | ⚠           | ⚠         | Same                                            |
| Get own user       | ✅         | ✅     | ✅       | ✅          | ✅        | Open                                            |
| Get another user   | ⚠          | ⚠      | ⚠        | ⚠           | ⚠         | Service authorization applies                   |
| Get all users      | ✅         | ✅     | ✅       | ✅          | ❌        | `TenantSupervisorOnly`                          |
| Get users by role  | ✅         | ✅     | ✅       | ❌          | ❌        | `TenantManagerOnly`                             |
| Soft delete user   | ✅         | ✅     | ✅       | ❌          | ❌        | `TenantManagerOnly`                             |
| Bulk soft delete   | ✅         | ✅     | ✅       | ❌          | ❌        | Same                                            |
| Restore user       | ✅         | ✅     | ✅       | ❌          | ❌        | Same                                            |
| Bulk restore       | ✅         | ✅     | ✅       | ❌          | ❌        | Same                                            |
| Hard delete        | ✅         | ❌     | ❌       | ❌          | ❌        | Superuser only                                  |

### Who can update whom

| Actor                  | Can Update                           |
| ---------------------- | ------------------------------------ |
| Superuser              | Admin, Manager, Supervisor, Employee |
| Admin                  | Manager, Supervisor, Employee        |
| Manager                | Supervisor, Employee                 |
| Supervisor (dept head) | Employees in own department only     |
| Employee               | Self only                            |

# User Images

| Function             | Superuser | Admin | Manager | Supervisor | Employee |
| -------------------- | --------- | ----- | ------- | ---------- | -------- |
| Update user images   | ✅       | ✅    | ✅     | ⚠          | ⚠       |
| View user images     | ⚠        | ⚠     | ⚠      | ⚠          | ⚠       |
| Download user images | ⚠        | ⚠     | ⚠      | ⚠          | ⚠       |
| Get all users images | ✅       | ✅    | ✅     | ❌         | ❌      |
| Download all images  | ✅       | ✅    | ✅     | ❌         | ❌      |
| Soft delete image    | ✅       | ✅    | ✅     | ❌         | ❌      |
| Restore image        | ✅       | ✅    | ✅     | ❌         | ❌      |
| Hard delete image    | ✅       | ❌    | ❌     | ❌         | ❌      |

# Audits

| Function     | Superuser | Admin | Manager | Supervisor | Employee |
| ------------ | --------- | ----- | ------- | ---------- | -------- |
| User audits  | ✅       | ✅    | ✅      | ❌        | ❌      |
| Image audits | ✅       | ✅    | ✅      | ❌        | ❌      |

# Department Module

| Function              | Superuser | Admin | Manager | Supervisor | Employee |
| --------------------- | --------- | ----- | ------- | ---------- | -------- |
| Import                | ✅       | ✅    | ✅      | ❌        | ❌       |
| Create                | ✅       | ✅    | ✅      | ❌        | ❌       |
| Update                | ✅       | ✅    | ✅      | ❌        | ❌       |
| View                  | ✅       | ✅    | ✅      | ✅        | ❌       |
| List                  | ✅       | ✅    | ✅      | ✅        | ❌       |
| View user departments | ✅       | ✅    | ✅      | ❌        | ❌       |
| Delete                | ✅       | ✅    | ✅      | ❌        | ❌       |
| Restore               | ✅       | ✅    | ✅      | ❌        | ❌       |
| Audits                | ✅       | ✅    | ✅      | ❌        | ❌       |

# Branch Module

(Changed materially)

| Function        | Superuser | Admin | Manager | Supervisor | Employee |
| --------------- | --------- | ----- | ------- | ---------- | -------- |
| Import branches | ✅       | ✅    | ✅      | ❌        | ❌       |
| Create branch   | ✅       | ✅    | ❌      | ❌        | ❌       |
| Read branches   | ✅       | ✅    | ✅      | ✅        | ✅       |
| Update branch   | ✅       | ✅    | ✅      | ❌        | ❌       |
| Delete branch   | ✅       | ✅    | ✅      | ❌        | ❌       |
| Restore branch  | ✅       | ✅    | ✅      | ❌        | ❌       |

---

# Role-to-role privilege matrix (`canManage()`)

| Acting Role → Target | Superuser | Admin | Manager | Supervisor | Employee |
| -------------------- | --------- | ----- | ------- | ---------- | -------- |
| Superuser            | ❌       | ✅    | ✅      | ✅        | ✅       |
| Admin                | ❌       | ❌    | ✅      | ✅        | ✅       |
| Manager              | ❌       | ❌    | ❌      | ✅        | ✅       |
| Supervisor           | ❌       | ❌    | ❌      | ❌        | ✅       |
| Employee             | ❌       | ❌    | ❌      | ❌        | ❌       |

Peers blocked.