import Image from 'next/image'
import Link from 'next/link'

export const metadata = {
  title: 'Scalus - Cardano Smart Contracts and dApps Development Platform',
  description: 'Write smart contracts, build transactions and application business logic — all in Scala 3, backed by industry-grade tools and great development experience.',
}

export default function IndexPage() {
  return (
    <div className="bg-white">

  <div className="relative isolate px-6">
    <div className="mx-auto max-w-6xl flex flex-col-reverse lg:flex-row items-center py-15 sm:py-22 lg:py-25">
      {/* Left: Text */}
      <div className="lg:w-2/3 text-center lg:text-left lg:pr-16 mt-12 lg:mt-0">
        <h1 className="text-5xl font-semibold tracking-tight text-balance text-gray-900 sm:text-6xl">Accelerate innovation on Cardano with Scalus</h1>
        <p className="mt-8 text-lg font-medium text-pretty text-gray-500 sm:text-xl/8">With Scalus DApps development platform you can: Write smart contracts, build transactions and application business logic — all in Scala 3, backed by industry-grade tools and great development experience.</p>
        <div className="mt-10 flex items-center justify-center lg:justify-start gap-x-6">
          <Link href="/docs/get-started" className="rounded-md bg-black px-3.5 py-2.5 text-sm font-semibold text-white shadow-xs hover:bg-gray-800 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-black">Get started</Link>
          <Link href="/docs" className="text-sm/6 font-semibold text-gray-900">Learn more <span aria-hidden="true">→</span></Link>
        </div>
      </div>
      {/* Right: Image */}
      <div className="lg:w-1/3 flex justify-center">
        <Image
          src="/scalus-logo-vertical-dark.png"
          alt="Scalus logo"
          width={300}
          height={300}
          priority
        />
      </div>
    </div>
  </div>
  {/* Features */}
  <div className="bg-gray-50 py-15 sm:py-22">
  <div className="mx-auto max-w-2xl px-6 lg:max-w-7xl lg:px-8">
    {/* <h2 className="text-center text-base/7 font-semibold text-indigo-600">Scalus - DApps development platform</h2> */}
    <h2 className="mx-auto text-center text-4xl font-semibold tracking-tight text-balance text-gray-950 sm:text-5xl">Everything you need to build on Cardano</h2>
    <div className="mt-10 grid gap-4 sm:mt-16 lg:grid-cols-3 lg:grid-rows-2">
      <div className="relative lg:row-span-2">
        <div className="absolute inset-px rounded-lg bg-white lg:rounded-l-4xl"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] lg:rounded-l-[calc(2rem+1px)]">
          <div className="px-8 pt-8 pb-3 sm:px-10 sm:pt-10 sm:pb-0">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center">Cardano Smart Contracts</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Scalus compiles a subset of Scala 3 code to Untyped Plutus Core (UPLC).</p>
          </div>
          
          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">
              <Image
                src="/scalus-sir-uplc.png"
                alt="Scalus smart contract to UPLC"
                className="size-full object-cover object-top"
                width={315} 
                height={475}
                priority
              />         
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 lg:rounded-l-4xl"></div>
      </div>
      <div className="relative max-lg:row-start-1">
        <div className="absolute inset-px rounded-lg bg-white max-lg:rounded-t-4xl"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] max-lg:rounded-t-[calc(2rem+1px)]">
          <div className="px-8 pt-8 sm:px-10 sm:pt-10">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center">Cardano Off-chain</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Scalus offers the tools necessary for building and evaluating transactions off-chain. They run on following platforms: </p>
          </div>
          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">
            
            <Image
                src="/scalus-off-chain.png"
                alt="Scalus off-chain library - JVM, JS, TS, LLVM"
                className="w-full max-lg:max-w-xs"
                width={315} 
                height={105}
                priority
              />        
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 max-lg:rounded-t-4xl"></div>
      </div>
      <div className="relative max-lg:row-start-3 lg:col-start-2 lg:row-start-2">
        <div className="absolute inset-px rounded-lg bg-white"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)]">
          <div className="px-8 pt-8 sm:px-10 sm:pt-10">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center">Cardano Layer 2 scaling solutions</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Scalus provides the reliable platform for scaling DApps on Cardano.</p>
          </div>
          <div className="@container flex flex-1 items-center max-lg:py-6 lg:pb-2">
            <img className="h-[min(152px,40cqw)] object-cover" src="https://tailwindcss.com/plus-assets/img/component-images/bento-03-security.png" alt="" />
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5"></div>
      </div>
      <div className="relative lg:row-span-2">
        <div className="absolute inset-px rounded-lg bg-white max-lg:rounded-b-4xl lg:rounded-r-4xl"></div>
        <div className="relative flex h-full flex-col overflow-hidden rounded-[calc(var(--radius-lg)+1px)] max-lg:rounded-b-[calc(2rem+1px)] lg:rounded-r-[calc(2rem+1px)]">
          <div className="px-8 pt-8 pb-3 sm:px-10 sm:pt-10 sm:pb-0">
            <p className="mt-2 text-lg font-medium tracking-tight text-gray-950 max-lg:text-center">Application Business Logic</p>
            <p className="mt-2 max-w-lg text-sm/6 text-gray-600 max-lg:text-center">Scala is well-positioned for building fast, concurrent, and distributed systems.</p>
          </div>
          <div className="flex flex-1 items-center justify-center px-8 max-lg:pt-10 max-lg:pb-12 sm:px-10 lg:pb-2">
              <Image
                src="/scala-app.png"
                alt="Scala rich ecosystem"
                className="size-full object-cover object-top"
                width={315} 
                height={475}
                priority
              />         
          </div>
        </div>
        <div className="pointer-events-none absolute inset-px rounded-lg shadow-sm ring-1 ring-black/5 max-lg:rounded-b-4xl lg:rounded-r-4xl"></div>
      </div>
    </div>
  </div>
</div>

{/* Trusted by */}
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
