import fs from 'fs';
import path from 'path';

const redirects = JSON.parse(fs.readFileSync(new URL('./redirects.json', import.meta.url)));
const outputDir = 'out';

for (const { from, to } of redirects) {
  const dir = path.join(outputDir, from.replace(/^\//, ''));
  const file = path.join(dir, 'index.html');

  if (fs.existsSync(file)) {
    console.warn(`⚠️  Skipping ${from} - page already exists`);
    continue;
  }

  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(file, `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta http-equiv="refresh" content="0; url=${to}">
  <link rel="canonical" href="${to}">
  <meta name="robots" content="noindex">
</head>
<body>Redirecting to <a href="${to}">${to}</a>...</body>
</html>`);
  console.log(`✓ ${from} → ${to}`);
}

if (redirects.length) console.log(`\n${redirects.length} redirect(s)`);
else console.log('No redirects configured.');
