import { Footer, Layout, Navbar } from 'nextra-theme-docs'
import { Banner, Head } from 'nextra/components'
import { getPageMap } from 'nextra/page-map'
import 'nextra-theme-docs/style.css'
import './globals.css'
import { GoogleAnalytics } from '@next/third-parties/google'
 
export const metadata = {
  // Define your metadata here
  // For more information on metadata API, see: https://nextjs.org/docs/app/building-your-application/optimizing/metadata
  metadataBase: new URL('https://scalus.org'),
  title: {
    template: '%s | Docs | Scalus'
  },
  description: 'Documentation for Scalus: Cardano Smart Contracts and dApps Development Platform',
  applicationName: 'Scalus',
  //generator: 'Next.js',
  appleWebApp: {
    title: 'Scalus'
  },
    other: {
    'msapplication-TileImage': '/ms-icon-144x144.png',
    'msapplication-TileColor': '#fff'
  },
  twitter: {
    site: 'https://scalus.org'
  }
}
 
const banner = <Banner storageKey="scalus-club"><a href="https://luma.com/scalus" target="_blank" rel="noopener noreferrer">Scalus Club is now open! Join us to get an early access to new features 🎉</a></Banner>

const navbar = (
  <Navbar
    logo={
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <img src="/scalus-logo-dark.png" alt="Logo" style={{ height: '2.5rem', marginRight: '0.5rem' }} />
        {/* <b>Scalus</b>{'   '}
        <span style={{ opacity: '60%' }}>DApps Development Platform for Cardano</span> */}
      </div>
    }
    projectLink="https://github.com/scalus3/scalus"
    // Lantr discord server / Scalus channel
    chatLink="https://discord.gg/B6tXmBzhTn"
  />
)
const footer = <Footer>Apache 2.0 © {new Date().getFullYear()} Lantr R&D Labs</Footer>
const pageMap = await getPageMap()
 
export default async function RootLayout({ children }) {
  return (
    <html
      // Not required, but good for SEO
      lang="en"
      // Required to be set
      dir="ltr"
      // Suggested by `next-themes` package https://github.com/pacocoursey/next-themes#with-app
      suppressHydrationWarning
    >
      <Head
      // ... Your additional head options
      >
        {/* Your additional tags should be passed as `children` of `<Head>` element */}
      </Head>
      <body>
        <Layout
          banner={banner}
          navbar={navbar}
          pageMap={await getPageMap()}
          docsRepositoryBase="https://github.com/scalus3/scalus"
          footer={footer}
          // editLink="Edit this page on GitHub"
          sidebar={{ defaultMenuCollapseLevel: 1 }}
          pageMap={pageMap}
          // ... Your additional layout options
          darkMode={false}
          nextThemes={{
            forcedTheme: 'light',
          }}
        >
          {children}
        </Layout>
      </body>
      <GoogleAnalytics gaId="G-JMC32W1C5Z" />
    </html>
  )
}