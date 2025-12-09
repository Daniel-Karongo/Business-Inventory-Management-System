import fs from "fs";
import path from "path";

// Paths
const assetsDir = path.join("src", "assets");
const iconsDir = path.join(assetsDir, "icons");

// SVG icon templates
const icons = {
  "dashboard.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M3 13h8V3H3v10zM13 21h8V11h-8v10zM13 3v4h8V3h-8zM3 21h8v-6H3v6z"/>
</svg>
  `,
  "inventory.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M3 7l9-4 9 4M4 7v10h16V7M9 10h6v6H9z"/>
</svg>
  `,
  "products.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M20 7v6l-8 7-8-7V7l8-4 8 4z"/>
</svg>
  `,
  "suppliers.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M3 7h18M5 7v10h14V7"/>
</svg>
  `,
  "sales.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M3 3h18v13H3zM7 21h10"/>
</svg>
  `,
  "payments.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M3 10h18M5 6h14v12H5zM9 14h6"/>
</svg>
  `,
  "customers.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M16 11a3 3 0 100-6 3 3 0 000 6zM8 11a3 3 0 100-6 3 3 0 000 6zM3 20a6 6 0 0112 0"/>
</svg>
  `,
  "accounts.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M12 8v8M8 12h8M3 21h18"/>
</svg>
  `,
  "users.svg": `
<svg width="24" height="24" stroke="currentColor" fill="none" stroke-width="2"
 stroke-linecap="round" stroke-linejoin="round" viewBox="0 0 24 24">
 <path d="M12 12a5 5 0 100-10 5 5 0 000 10zM4 20a8 8 0 0116 0"/>
</svg>
  `
};

// Ensure src/assets exists
if (!fs.existsSync(assetsDir)) {
  fs.mkdirSync(assetsDir, { recursive: true });
}

// Create icons folder
if (!fs.existsSync(iconsDir)) {
  fs.mkdirSync(iconsDir, { recursive: true });
}

// Write all icons
Object.entries(icons).forEach(([filename, content]) => {
  fs.writeFileSync(path.join(iconsDir, filename), content.trim());
});

console.log("✅ Icons installed into src/assets/icons/");