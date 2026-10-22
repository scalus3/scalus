/** @type {import('next-sitemap').IConfig} */
module.exports = {
    siteUrl: process.env.SITE_URL || 'https://scalus.org/',
    generateRobotsTxt: true,
    robotsTxtOptions: {
      policies: [
        // /api/<version>/ and /llms/<version>/ duplicate the root API and llms files, which show
        // the latest version. Search results then point to the latest only. A 2.x release needs
        // one more line. Spec [VS-1] [VS-2]: docs/superpowers/specs/2026-10-22-versioned-api-docs-design.md
        { userAgent: '*', allow: '/', disallow: ['/api/1.', '/llms/'] },
      ],
    },
    outDir: 'out',
    generateIndexSitemap: false,
  }
