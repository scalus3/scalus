import Image from 'next/image'
import Link from 'next/link'

export const metadata = {
  title: 'Cardano Smart Contracts & dApps Development Platform | Scalus Docs',
  description: 'Professional Cardano smart contracts and dApps development in Scala 3. Scalus compiles to Plutus Core for on-chain validation with full-stack Cardano development: write smart contracts, build transactions, and application logic in one unified platform.',
  keywords: 'Cardano smart contracts, Cardano dApps development, Plutus smart contracts, Cardano blockchain development, UPLC compiler, Cardano developer platform, Scala blockchain, Cardano full-stack development',
  openGraph: {
    title: 'Cardano Smart Contracts & dApps Development Platform | Scalus Docs',
    description: 'Professional Cardano smart contracts and dApps development in Scala 3. Write Plutus contracts, build transactions, and create full-stack Cardano applications.',
    url: 'https://scalus.org',
    siteName: 'Scalus',
    type: 'website',
    images: [
      {
        url: 'https://scalus.org/scalus-logo-vertical-dark.png',
        width: 300,
        height: 300,
        alt: 'Scalus - Cardano Smart Contracts and dApps Development Platform',
      },
    ],
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Cardano Smart Contracts & dApps Development Platform | Scalus',
    description: 'Professional Cardano smart contracts and dApps development in Scala 3. Write Plutus contracts, build transactions, and create full-stack Cardano applications.',
    images: ['https://scalus.org/scalus-logo-vertical-dark.png'],
  },
}

export default function IndexPage() {
  // FAQ data - single source of truth
  const faqItems = [
    {
      question: "What is Scalus and how does it help with Cardano smart contract development?",
      answer: "Scalus is a comprehensive platform for developing Cardano smart contracts and dApps using Scala 3. It compiles Scala code to Untyped Plutus Core (UPLC), the language that runs on the Cardano blockchain. This allows developers to write type-safe, modern smart contracts with the full power of Scala's ecosystem.",
      answerWithLinks: null // plain text only
    },
    {
      question: "Why choose Scalus for Cardano dApps development?",
      answer: "Scalus offers a unified development experience where you can write smart contracts, build transactions, and develop application business logic all in Scala 3. This eliminates context switching between different languages and provides access to industry-grade tools, strong type safety, and a rich ecosystem for full-stack Cardano application development.",
      answerWithLinks: null
    },
    {
      question: "Can I use Scalus to deploy smart contracts on Cardano mainnet?",
      answer: "Yes, Scalus compiles to standard UPLC code that can be deployed to Cardano mainnet, testnet, or any Cardano network. The compiled smart contracts are fully compatible with the Cardano blockchain and can interact with other Cardano smart contracts and dApps.",
      answerWithLinks: null
    },
    {
      question: "What platforms does Scalus support for Cardano dApps development?",
      answer: "Scalus is cross-platform and supports JVM, JavaScript, TypeScript, and Native (LLVM) environments. This flexibility allows you to build Cardano dApps that run on servers, browsers, and native applications, all using the same codebase.",
      answerWithLinks: null
    },
    {
      question: "How do I get started with Cardano smart contract development in Scalus?",
      answer: "Getting started is easy! Visit the Get Started guide at https://scalus.org/docs/get-started to set up your development environment. You'll learn how to write your first Cardano smart contract, compile it to UPLC, and build transactions.",
      answerWithLinks: (
        <>Getting started is easy! Visit our <Link href="/docs/get-started" className="text-blue-600 hover:underline">Get Started guide</Link> to set up your development environment. You'll learn how to write your first Cardano smart contract, compile it to UPLC, and build transactions. Our <Link href="/docs" className="text-blue-600 hover:underline">documentation</Link> provides comprehensive tutorials and examples.</>
      )
    },
    {
      question: "Is Scalus suitable for professional Cardano blockchain development?",
      answer: "Absolutely. Scalus is designed for professional-grade Cardano development with industry-standard tools, comprehensive testing capabilities, and production-ready features. It's suitable for everything from simple smart contracts to complex, full-stack Cardano dApps with sophisticated business logic.",
      answerWithLinks: null
    }
  ];

  const schemaOrgData = {
    "@context": "https://schema.org",
    "@type": "SoftwareApplication",
    "name": "Scalus",
    "applicationCategory": "DeveloperApplication",
    "operatingSystem": "Cross-platform",
    "description": "Professional Cardano smart contracts and dApps development platform in Scala 3. Compile to Plutus Core for on-chain validation with full-stack Cardano development capabilities.",
    "url": "https://scalus.org",
    "softwareVersion": "1.0",
    "offers": {
      "@type": "Offer",
      "price": "0",
      "priceCurrency": "USD"
    },
    "keywords": "Cardano smart contracts, Cardano dApps development, blockchain development, Scala blockchain, UPLC compiler",
    "programmingLanguage": "Scala 3",
    "featureList": [
      "Cardano smart contract development",
      "Cardano dApps development platform",
      "Transaction building and evaluation",
      "Full-stack blockchain application development",
      "Cross-platform support (JVM, JavaScript, Native)"
    ]
  };

  const faqSchemaData = {
    "@context": "https://schema.org",
    "@type": "FAQPage",
    "mainEntity": faqItems.map(item => ({
      "@type": "Question",
      "name": item.question,
      "acceptedAnswer": {
        "@type": "Answer",
        "text": item.answer
      }
    }))
  };

  return (
    <div className="bg-white">
      <script
        type="application/ld+json"
        dangerouslySetInnerHTML={{ __html: JSON.stringify(schemaOrgData) }}
      />
      <script
        type="application/ld+json"
        dangerouslySetInnerHTML={{ __html: JSON.stringify(faqSchemaData) }}
      />

  <div className="relative isolate px-6">
    <div className="mx-auto max-w-6xl flex flex-col-reverse lg:flex-row items-center pt-12 pb-6 sm:pt-14 sm:pb-8 lg:pt-16 lg:pb-8">
      {/* Left: Text */}
      <div className="lg:w-2/3 text-center lg:text-left lg:pr-16 mt-12 lg:mt-0">
      <h1 className="text-4xl font-semibold tracking-tight text-balance text-gray-900 sm:text-5xl">Build Cardano Smart Contracts & dApps with Scala 3</h1>
      <p className="mt-8 text-lg font-medium text-pretty text-gray-500 sm:text-xl/8">Scalus is a professional Cardano smart contracts and dApps development platform for Scala 3 developers. Write and test Cardano smart contracts, build transactions, and ship full-stack Cardano applications — all in pure Scala 3 with industry-grade tools and an exceptional developer experience.</p>
        <div className="mt-10 flex items-center justify-center lg:justify-start gap-x-6">
          <Link href="/docs/get-started" className="rounded-md bg-black px-3.5 py-2.5 text-sm font-semibold text-white shadow-xs hover:bg-gray-800 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-black">Get started</Link>
          <Link href="/docs" className="text-sm/6 font-semibold text-gray-900">Learn more <span aria-hidden="true">→</span></Link>
        </div>
      </div>
      {/* Right: Image */}
      <div className="lg:w-1/3 flex justify-center">
        <Image
          src="/scalus-logo-vertical-dark.png"
          alt="Scalus - Cardano Smart Contracts and dApps Development Platform"
          width={300}
          height={300}
          priority
        />
      </div>
    </div>
  </div>

  {/* Quick Start Links */}
  <div className="bg-white py-8 sm:py-10">
    <div className="mx-auto max-w-7xl px-6 lg:px-8">
      <div className="mx-auto max-w-2xl text-center mb-12">
        <h2 className="text-3xl font-semibold tracking-tight text-gray-950 sm:text-4xl">Start Building on Cardano</h2>
        <p className="mt-4 text-lg text-gray-600">Everything you need to build production-ready Cardano smart contracts and dApps</p>
      </div>
      <div className="mx-auto grid max-w-5xl grid-cols-1 gap-6 sm:grid-cols-3">
        <Link href="/docs/get-started" className="group relative overflow-hidden rounded-2xl bg-gradient-to-br from-blue-50 to-indigo-50 p-8 transition-all hover:shadow-lg hover:scale-105">
          <div className="flex flex-col h-full">
            <div className="flex items-center justify-center w-12 h-12 rounded-lg bg-blue-600 text-white mb-4">
              <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
              </svg>
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">Get Started</h3>
            <p className="text-gray-600 flex-grow">Set up your development environment and write your first Cardano smart contract in minutes</p>
            <div className="mt-4 flex items-center text-blue-600 font-semibold group-hover:translate-x-1 transition-transform">
              Start now <span aria-hidden="true" className="ml-2">→</span>
            </div>
          </div>
        </Link>

        <Link href="/docs/smart-contract/developing-smart-contracts" className="group relative overflow-hidden rounded-2xl bg-gradient-to-br from-purple-50 to-pink-50 p-8 transition-all hover:shadow-lg hover:scale-105">
          <div className="flex flex-col h-full">
            <div className="flex items-center justify-center w-12 h-12 rounded-lg bg-purple-600 text-white mb-4">
              <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
              </svg>
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">Build Smart Contracts</h3>
            <p className="text-gray-600 flex-grow">Learn how to write, compile, and deploy Cardano smart contracts using Scala 3</p>
            <div className="mt-4 flex items-center text-purple-600 font-semibold group-hover:translate-x-1 transition-transform">
              Learn more <span aria-hidden="true" className="ml-2">→</span>
            </div>
          </div>
        </Link>

        <Link href="/docs/transaction-builder/building-first-transaction" className="group relative overflow-hidden rounded-2xl bg-gradient-to-br from-green-50 to-emerald-50 p-8 transition-all hover:shadow-lg hover:scale-105">
          <div className="flex flex-col h-full">
            <div className="flex items-center justify-center w-12 h-12 rounded-lg bg-green-600 text-white mb-4">
              <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
              </svg>
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">Build Transactions</h3>
            <p className="text-gray-600 flex-grow">Create and submit Cardano transactions with our type-safe transaction builder API</p>
            <div className="mt-4 flex items-center text-green-600 font-semibold group-hover:translate-x-1 transition-transform">
              Explore API <span aria-hidden="true" className="ml-2">→</span>
            </div>
          </div>
        </Link>
      </div>
    </div>
  </div>

  {/* Features */}
  <div className="bg-gray-50 py-15 sm:py-17">
  <div className="mx-auto max-w-2xl px-6 lg:max-w-7xl lg:px-8">
    <p className="text-center text-base/7 font-semibold text-gray-600">Complete Cardano Development Platform</p>
    <h2 className="mx-auto mt-2 text-center text-3xl font-semibold tracking-tight text-balance text-gray-950 sm:text-4xl">All-in-One Platform for Cardano Development</h2>
    <p className="mx-auto mt-6 max-w-4xl text-center text-lg text-gray-600">Scalus provides a unified Cardano development platform for building smart contracts and decentralized applications in Scala 3. Use a single, professional toolchain to design, test, and deploy Cardano dApps and on-chain code with confidence.</p>
    <div className="mt-10 grid gap-4 sm:mt-16 lg:grid-cols-3 lg:grid-rows-2">
      <Link href="/docs/smart-contract" className="relative lg:row-span-2 group">
        <div className="absolute inset-px rounded-lg bg-white lg:rounded-l-4xl transition-colors group-hover:bg-blue-50"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] lg:rounded-l-[calc(2rem+1px)]">
          <div className="px-8 pt-8 pb-3 sm:px-10 sm:pt-10 sm:pb-0">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center group-hover:text-blue-600 transition-colors">Cardano Smart Contracts Development</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Write Cardano smart contracts in Scala 3. Scalus compiles your code to Untyped Plutus Core (UPLC) for on-chain execution, providing type safety and modern language features for blockchain development.</p>
          </div>

          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">
              <Image
                src="/scalus-sir-uplc.png"
                alt="Cardano smart contract compilation from Scala to UPLC - Scalus compiler workflow"
                className="object-cover object-top"
                width={315}
                height={475}
                priority
              />
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 lg:rounded-l-4xl group-hover:ring-blue-600/20"></div>
      </Link>
      <Link href="/docs/transaction-builder" className="relative max-lg:row-start-1 group">
        <div className="absolute inset-px rounded-lg bg-white max-lg:rounded-t-4xl transition-colors group-hover:bg-purple-50"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] max-lg:rounded-t-[calc(2rem+1px)]">
          <div className="px-8 pt-8 sm:px-10 sm:pt-10">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center group-hover:text-purple-600 transition-colors">Cardano dApps Off-chain Development</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Build complete Cardano dApps with off-chain transaction building and evaluation tools. Scalus runs on multiple platforms for flexible deployment: JVM, JavaScript, TypeScript, and native.</p>
          </div>
          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">

            <Image
                src="/scalus-off-chain.png"
                alt="Cardano dApps off-chain development - Cross-platform support for JVM, JavaScript, TypeScript, and Native"
                className="object-cover object-top"
                width={315}
                height={105}
                priority
              />
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 max-lg:rounded-t-4xl group-hover:ring-purple-600/20"></div>
      </Link>
      <Link href="/docs" className="relative max-lg:row-start-3 lg:col-start-2 lg:row-start-2 group">
        <div className="absolute inset-px rounded-lg bg-white transition-colors group-hover:bg-green-50"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)]">
          <div className="px-8 pt-8 sm:px-10 sm:pt-10">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center group-hover:text-green-600 transition-colors">Cardano Layer 2 Scaling Solutions</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Scale your Cardano dApps with Layer 2 solutions. Scalus provides a reliable foundation for building high-performance, scalable decentralized applications on Cardano blockchain.</p>
          </div>
          <div className="@container flex flex-1 items-center max-lg:py-6 lg:pb-2">
            <img className="h-[min(152px,40cqw)] object-cover" src="https://tailwindcss.com/plus-assets/img/component-images/bento-03-security.png" alt="Cardano Layer 2 scaling solutions for high-performance dApps" />
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 group-hover:ring-green-600/20"></div>
      </Link>
      <Link href="/docs/language-guide/scala3" className="relative lg:row-span-2 group">
        <div className="absolute inset-px rounded-lg bg-white max-lg:rounded-b-4xl lg:rounded-r-4xl transition-colors group-hover:bg-amber-50"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] max-lg:rounded-b-[calc(2rem+1px)] lg:rounded-r-[calc(2rem+1px)]">
          <div className="px-8 pt-8 pb-3 sm:px-10 sm:pt-10 sm:pb-0">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center group-hover:text-amber-600 transition-colors">Full-Stack Cardano Application Development</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Build complete Cardano dApps with business logic in Scala 3. Leverage Scala's powerful ecosystem for fast, concurrent, and distributed systems alongside your Cardano smart contracts.</p>
          </div>
          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">
              <Image
                src="/scala-app.png"
                alt="Full-stack Cardano dApps development with Scala ecosystem - build business logic and applications"
                className="size-full object-cover object-top"
                width={315}
                height={475}
                priority
              />
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 max-lg:rounded-b-4xl lg:rounded-r-4xl group-hover:ring-amber-600/20"></div>
      </Link>
    </div>
  </div>
</div>

{/* FAQ Section */}
<div className="bg-white py-15 sm:py-22">
  <div className="mx-auto max-w-4xl px-6 lg:px-8">
    <h2 className="text-center text-3xl font-semibold tracking-tight text-gray-950 sm:text-4xl mb-4">Frequently Asked Questions</h2>
    <p className="text-center text-lg text-gray-600 mb-12">Learn more about Cardano smart contracts and dApps development with Scalus</p>

    <div className="space-y-8">
      {faqItems.map((faq, index) => (
        <details key={index} className="group rounded-lg bg-gray-50 p-6">
          <summary className="flex cursor-pointer items-center justify-between text-lg font-semibold text-gray-900">
            {faq.question}
            <span className="ml-4 flex-shrink-0 text-gray-400 group-open:rotate-180 transition-transform">▼</span>
          </summary>
          <p className="mt-4 text-gray-600">
            {faq.answerWithLinks || faq.answer}
          </p>
        </details>
      ))}
    </div>
  </div>
</div>

{/* Trusted by section - hidden for now
<div className="bg-white py-15 sm:py-22">
  <div className="mx-auto max-w-7xl px-6 lg:px-8">
    <h2 className="text-center text-lg/8 font-semibold text-gray-900 sm:text-1xl ">Trusted by the world's most innovative teams</h2> <br />

    <div className="mx-auto mt-10 grid max-w-lg grid-cols-4 items-center gap-x-8 gap-y-10 sm:max-w-xl sm:grid-cols-6 sm:gap-x-10 lg:mx-0 lg:max-w-none lg:grid-cols-5">
      <img className="col-span-2 max-h-12 w-full object-contain lg:col-span-1" src="https://tailwindcss.com/plus-assets/img/logos/158x48/transistor-logo-gray-900.svg" alt="Transistor" width="158" height="48" />
      <img className="col-span-2 max-h-12 w-full object-contain lg:col-span-1" src="https://tailwindcss.com/plus-assets/img/logos/158x48/reform-logo-gray-900.svg" alt="Reform" width="158" height="48" />
      <img className="col-span-2 max-h-12 w-full object-contain lg:col-span-1" src="https://tailwindcss.com/plus-assets/img/logos/158x48/tuple-logo-gray-900.svg" alt="Tuple" width="158" height="48" />
      <img className="col-span-2 max-h-12 w-full object-contain sm:col-start-2 lg:col-span-1" src="https://tailwindcss.com/plus-assets/img/logos/158x48/savvycal-logo-gray-900.svg" alt="SavvyCal" width="158" height="48" />
      <img className="col-span-2 col-start-2 max-h-12 w-full object-contain sm:col-start-auto lg:col-span-1" src="https://tailwindcss.com/plus-assets/img/logos/158x48/statamic-logo-gray-900.svg" alt="Statamic" width="158" height="48" />
    </div>
  </div>
</div>
*/}

</div>
  )
}
