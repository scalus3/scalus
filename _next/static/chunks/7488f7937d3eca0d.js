(globalThis.TURBOPACK||(globalThis.TURBOPACK=[])).push(["object"==typeof document?document.currentScript:void 0,50604,e=>{"use strict";var t=e.i(44366),i=(0,t.__name)((e,i,l,r)=>{e.attr("class",l);let{width:a,height:s,x:c,y:d}=o(e,i);(0,t.configureSvgSize)(e,s,a,r);let g=n(c,d,a,s,i);e.attr("viewBox",g),t.log.debug(`viewBox configured: ${g} with padding: ${i}`)},"setupViewPortForSVG"),o=(0,t.__name)((e,t)=>{let i=e.node()?.getBBox()||{width:0,height:0,x:0,y:0};return{width:i.width+2*t,height:i.height+2*t,x:i.x,y:i.y}},"calculateDimensionsWithPadding"),n=(0,t.__name)((e,t,i,o,n)=>`${e-n} ${t-n} ${i} ${o}`,"createViewBox");e.s(["setupViewPortForSVG",()=>i])},25628,e=>{"use strict";var t=e.i(44366);e.i(47716);var i=e.i(23685),o=(0,t.__name)((e,t)=>{let o;return"sandbox"===t&&(o=(0,i.select)("#i"+e)),("sandbox"===t?(0,i.select)(o.nodes()[0].contentDocument.body):(0,i.select)("body")).select(`[id="${e}"]`)},"getDiagramElement");e.s(["getDiagramElement",()=>o])},6093,e=>{"use strict";var t=(0,e.i(44366).__name)(()=>`
  /* Font Awesome icon styling - consolidated */
  .label-icon {
    display: inline-block;
    height: 1em;
    overflow: visible;
    vertical-align: -0.125em;
  }
  
  .node .label-icon path {
    fill: currentColor;
    stroke: revert;
    stroke-width: revert;
  }
`,"getIconStyles");e.s(["getIconStyles",()=>t])},69696,e=>{"use strict";var t=e.i(95727),i=e.i(26290);e.s(["channel",0,(e,o)=>t.default.lang.round(i.default.parse(e)[o])],69696)}]);