import fs from 'fs';
import path from 'path';

/**
 * Redirect configuration for scalus-site
 *
 * Add redirects here when changing URL structure.
 * Format: { from: '/old-path', to: '/new-path' }
 *
 * The 'from' path should NOT include trailing slashes.
 * The 'to' path should be the full new path.
 *
 * Examples:
 *   { from: '/docs/old-guide', to: '/docs/new-guide' }
 *   { from: '/tutorial', to: '/docs/get-started' }
 */
const redirects = [
  // Add your redirects here
  // { from: '/old-path', to: '/new-path' },
];

const outputDir = 'out';

function generateRedirectHtml(to) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Redirecting...</title>
  <meta http-equiv="refresh" content="0; url=${to}">
  <link rel="canonical" href="${to}">
  <meta name="robots" content="noindex">
  <script>window.location.replace("${to}");</script>
</head>
<body>
  <p>Redirecting to <a href="${to}">${to}</a>...</p>
</body>
</html>`;
}

function generateRedirects() {
  if (redirects.length === 0) {
    console.log('No redirects configured.');
    return;
  }

  let created = 0;
  let skipped = 0;

  for (const { from, to } of redirects) {
    const fromPath = from.replace(/^\//, ''); // Remove leading slash
    const targetDir = path.join(outputDir, fromPath);
    const targetFile = path.join(targetDir, 'index.html');

    // Check if a real page already exists at this path
    if (fs.existsSync(targetFile)) {
      console.warn(`⚠️  Skipping ${from} - page already exists`);
      skipped++;
      continue;
    }

    // Create directory and write redirect file
    fs.mkdirSync(targetDir, { recursive: true });
    fs.writeFileSync(targetFile, generateRedirectHtml(to));
    console.log(`✓ ${from} → ${to}`);
    created++;
  }

  console.log(`\nGenerated ${created} redirect(s)${skipped > 0 ? `, skipped ${skipped}` : ''}`);
}

generateRedirects();
