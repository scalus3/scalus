// Generates out/llms-full.txt and per-page markdown copies under out/docs/.
// Runs from scalus-site/ as part of postbuild. Fails loudly on empty input.
import fs from 'fs';
import path from 'path';
import { pathToFileURL } from 'url';

const CONTENT = 'content';
const OUT = 'out';
const SITE = 'https://scalus.org';

async function orderedEntries(dir) {
  const names = fs.readdirSync(dir).filter(n => !n.startsWith('_') && !n.startsWith('.'));
  const metaPath = path.join(dir, '_meta.js');
  let order = [];
  if (fs.existsSync(metaPath)) {
    const meta = (await import(pathToFileURL(metaPath))).default;
    order = Object.keys(meta);
  }
  const rank = n => {
    const base = n.replace(/\.mdx$/, '');
    const i = order.indexOf(base);
    return i === -1 ? order.length : i;
  };
  return names.sort((a, b) => rank(a) - rank(b) || a.localeCompare(b));
}

// Strips frontmatter, top-level import/export lines, and pure-JSX lines.
// Lines inside ``` fences are kept verbatim.
export function mdxToMarkdown(src) {
  let lines = src.split('\n');
  if (lines[0] === '---') {
    const end = lines.indexOf('---', 1);
    if (end !== -1) lines = lines.slice(end + 1);
  }
  const outLines = [];
  let inFence = false;
  for (const line of lines) {
    if (/^\s*(```|~~~)/.test(line)) { inFence = !inFence; outLines.push(line); continue; }
    if (inFence) { outLines.push(line); continue; }
    if (/^import\s/.test(line) || /^export\s/.test(line)) continue;
    if (/^\s*<\/?[A-Z][A-Za-z0-9.]*[^>]*\/?>?\s*$/.test(line)) continue; // pure JSX tag line
    outLines.push(line);
  }
  return outLines.join('\n').replace(/\n{3,}/g, '\n\n').trim() + '\n';
}

async function collectPages(dir, urlPath) {
  const pages = [];
  for (const name of await orderedEntries(dir)) {
    const full = path.join(dir, name);
    if (fs.statSync(full).isDirectory()) {
      pages.push(...await collectPages(full, `${urlPath}/${name}`));
    } else if (name.endsWith('.mdx')) {
      const base = name.replace(/\.mdx$/, '');
      const url = base === 'index' ? urlPath : `${urlPath}/${base}`;
      pages.push({ url, md: mdxToMarkdown(fs.readFileSync(full, 'utf8')) });
    }
  }
  return pages;
}

const pages = await collectPages(CONTENT, '/docs');
if (pages.length < 10) {
  console.error(`generate-llms: only ${pages.length} pages found, aborting`);
  process.exit(1);
}

let full = `# Scalus documentation (full)\n\nGenerated from ${SITE}. One section per page; each section header is the canonical URL.\n`;
for (const { url, md } of pages) {
  full += `\n\n---\nSource: ${SITE}${url}\n---\n\n${md}`;
  const mdPath = path.join(OUT, url.replace(/^\//, '') + '.md');
  fs.mkdirSync(path.dirname(mdPath), { recursive: true });
  fs.writeFileSync(mdPath, `Source: ${SITE}${url}\n\n${md}`);
}
fs.mkdirSync(OUT, { recursive: true });
fs.writeFileSync(path.join(OUT, 'llms-full.txt'), full);
console.log(`generate-llms: ${pages.length} pages → ${OUT}/llms-full.txt`);
