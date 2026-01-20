import { generateStaticParamsFor, importPage } from 'nextra/pages'
import { useMDXComponents as getMDXComponents } from '../../../mdx-components'

export const generateStaticParams = generateStaticParamsFor('mdxPath')

export async function generateMetadata(props) {
  const params = await props.params
  const { metadata } = await importPage(params.mdxPath)
  return metadata
}

const Wrapper = getMDXComponents().wrapper

function TechArticleSchema({ metadata, path }) {
  const url = `https://scalus.org/docs${path ? '/' + path.join('/') : ''}`
  const schema = {
    "@context": "https://schema.org",
    "@type": "TechArticle",
    "headline": metadata?.title || "Scalus Documentation",
    "description": metadata?.description || "Cardano smart contracts and dApps development with Scala 3",
    "url": url,
    "author": {
      "@type": "Organization",
      "name": "Scalus",
      "url": "https://scalus.org"
    },
    "publisher": {
      "@type": "Organization",
      "name": "Scalus",
      "url": "https://scalus.org"
    },
    "proficiencyLevel": "Beginner",
    "keywords": "Cardano, smart contracts, Scala 3, blockchain development, Plutus, UPLC, dApps"
  }

  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{ __html: JSON.stringify(schema) }}
    />
  )
}

export default async function Page(props) {
  const params = await props.params
  const result = await importPage(params.mdxPath)
  const { default: MDXContent, toc, metadata } = result
  return (
    <>
      <TechArticleSchema metadata={metadata} path={params.mdxPath} />
      <Wrapper toc={toc} metadata={metadata}>
        <MDXContent {...props} params={params} />
      </Wrapper>
    </>
  )
}