(globalThis.TURBOPACK||(globalThis.TURBOPACK=[])).push(["object"==typeof document?document.currentScript:void 0,50604,t=>{"use strict";var e=t.i(44366),s=(0,e.__name)((t,s,n,a)=>{t.attr("class",n);let{width:o,height:l,x:c,y:h}=i(t,s);(0,e.configureSvgSize)(t,l,o,a);let d=r(c,h,o,l,s);t.attr("viewBox",d),e.log.debug(`viewBox configured: ${d} with padding: ${s}`)},"setupViewPortForSVG"),i=(0,e.__name)((t,e)=>{let s=t.node()?.getBBox()||{width:0,height:0,x:0,y:0};return{width:s.width+2*e,height:s.height+2*e,x:s.x,y:s.y}},"calculateDimensionsWithPadding"),r=(0,e.__name)((t,e,s,i,r)=>`${t-r} ${e-r} ${s} ${i}`,"createViewBox");t.s(["setupViewPortForSVG",()=>s])},25628,t=>{"use strict";var e=t.i(44366);t.i(47716);var s=t.i(23685),i=(0,e.__name)((t,e)=>{let i;return"sandbox"===e&&(i=(0,s.select)("#i"+t)),("sandbox"===e?(0,s.select)(i.nodes()[0].contentDocument.body):(0,s.select)("body")).select(`[id="${t}"]`)},"getDiagramElement");t.s(["getDiagramElement",()=>i])},17653,t=>{"use strict";var e=t.i(25628),s=t.i(50604),i=t.i(42135),r=t.i(18873),n=t.i(44366),a=function(){var t=(0,n.__name)(function(t,e,s,i){for(s=s||{},i=t.length;i--;s[t[i]]=e);return s},"o"),e=[1,2],s=[1,3],i=[1,4],r=[2,4],a=[1,9],o=[1,11],l=[1,16],c=[1,17],h=[1,18],d=[1,19],u=[1,33],p=[1,20],g=[1,21],y=[1,22],m=[1,23],f=[1,24],_=[1,26],S=[1,27],b=[1,28],T=[1,29],k=[1,30],E=[1,31],C=[1,32],D=[1,35],x=[1,36],$=[1,37],v=[1,38],A=[1,34],I=[1,4,5,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,41,45,48,51,52,53,54,57],L=[1,4,5,14,15,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,39,40,41,45,48,51,52,53,54,57],w=[4,5,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,41,45,48,51,52,53,54,57],N={trace:(0,n.__name)(function(){},"trace"),yy:{},symbols_:{error:2,start:3,SPACE:4,NL:5,SD:6,document:7,line:8,statement:9,classDefStatement:10,styleStatement:11,cssClassStatement:12,idStatement:13,DESCR:14,"-->":15,HIDE_EMPTY:16,scale:17,WIDTH:18,COMPOSIT_STATE:19,STRUCT_START:20,STRUCT_STOP:21,STATE_DESCR:22,AS:23,ID:24,FORK:25,JOIN:26,CHOICE:27,CONCURRENT:28,note:29,notePosition:30,NOTE_TEXT:31,direction:32,acc_title:33,acc_title_value:34,acc_descr:35,acc_descr_value:36,acc_descr_multiline_value:37,CLICK:38,STRING:39,HREF:40,classDef:41,CLASSDEF_ID:42,CLASSDEF_STYLEOPTS:43,DEFAULT:44,style:45,STYLE_IDS:46,STYLEDEF_STYLEOPTS:47,class:48,CLASSENTITY_IDS:49,STYLECLASS:50,direction_tb:51,direction_bt:52,direction_rl:53,direction_lr:54,eol:55,";":56,EDGE_STATE:57,STYLE_SEPARATOR:58,left_of:59,right_of:60,$accept:0,$end:1},terminals_:{2:"error",4:"SPACE",5:"NL",6:"SD",14:"DESCR",15:"-->",16:"HIDE_EMPTY",17:"scale",18:"WIDTH",19:"COMPOSIT_STATE",20:"STRUCT_START",21:"STRUCT_STOP",22:"STATE_DESCR",23:"AS",24:"ID",25:"FORK",26:"JOIN",27:"CHOICE",28:"CONCURRENT",29:"note",31:"NOTE_TEXT",33:"acc_title",34:"acc_title_value",35:"acc_descr",36:"acc_descr_value",37:"acc_descr_multiline_value",38:"CLICK",39:"STRING",40:"HREF",41:"classDef",42:"CLASSDEF_ID",43:"CLASSDEF_STYLEOPTS",44:"DEFAULT",45:"style",46:"STYLE_IDS",47:"STYLEDEF_STYLEOPTS",48:"class",49:"CLASSENTITY_IDS",50:"STYLECLASS",51:"direction_tb",52:"direction_bt",53:"direction_rl",54:"direction_lr",56:";",57:"EDGE_STATE",58:"STYLE_SEPARATOR",59:"left_of",60:"right_of"},productions_:[0,[3,2],[3,2],[3,2],[7,0],[7,2],[8,2],[8,1],[8,1],[9,1],[9,1],[9,1],[9,1],[9,2],[9,3],[9,4],[9,1],[9,2],[9,1],[9,4],[9,3],[9,6],[9,1],[9,1],[9,1],[9,1],[9,4],[9,4],[9,1],[9,2],[9,2],[9,1],[9,5],[9,5],[10,3],[10,3],[11,3],[12,3],[32,1],[32,1],[32,1],[32,1],[55,1],[55,1],[13,1],[13,1],[13,3],[13,3],[30,1],[30,1]],performAction:(0,n.__name)(function(t,e,s,i,r,n,a){var o=n.length-1;switch(r){case 3:return i.setRootDoc(n[o]),n[o];case 4:this.$=[];break;case 5:"nl"!=n[o]&&(n[o-1].push(n[o]),this.$=n[o-1]);break;case 6:case 7:case 12:this.$=n[o];break;case 8:this.$="nl";break;case 13:let l=n[o-1];l.description=i.trimColon(n[o]),this.$=l;break;case 14:this.$={stmt:"relation",state1:n[o-2],state2:n[o]};break;case 15:let c=i.trimColon(n[o]);this.$={stmt:"relation",state1:n[o-3],state2:n[o-1],description:c};break;case 19:this.$={stmt:"state",id:n[o-3],type:"default",description:"",doc:n[o-1]};break;case 20:var h=n[o],d=n[o-2].trim();if(n[o].match(":")){var u=n[o].split(":");h=u[0],d=[d,u[1]]}this.$={stmt:"state",id:h,type:"default",description:d};break;case 21:this.$={stmt:"state",id:n[o-3],type:"default",description:n[o-5],doc:n[o-1]};break;case 22:this.$={stmt:"state",id:n[o],type:"fork"};break;case 23:this.$={stmt:"state",id:n[o],type:"join"};break;case 24:this.$={stmt:"state",id:n[o],type:"choice"};break;case 25:this.$={stmt:"state",id:i.getDividerId(),type:"divider"};break;case 26:this.$={stmt:"state",id:n[o-1].trim(),note:{position:n[o-2].trim(),text:n[o].trim()}};break;case 29:this.$=n[o].trim(),i.setAccTitle(this.$);break;case 30:case 31:this.$=n[o].trim(),i.setAccDescription(this.$);break;case 32:this.$={stmt:"click",id:n[o-3],url:n[o-2],tooltip:n[o-1]};break;case 33:this.$={stmt:"click",id:n[o-3],url:n[o-1],tooltip:""};break;case 34:case 35:this.$={stmt:"classDef",id:n[o-1].trim(),classes:n[o].trim()};break;case 36:this.$={stmt:"style",id:n[o-1].trim(),styleClass:n[o].trim()};break;case 37:this.$={stmt:"applyClass",id:n[o-1].trim(),styleClass:n[o].trim()};break;case 38:i.setDirection("TB"),this.$={stmt:"dir",value:"TB"};break;case 39:i.setDirection("BT"),this.$={stmt:"dir",value:"BT"};break;case 40:i.setDirection("RL"),this.$={stmt:"dir",value:"RL"};break;case 41:i.setDirection("LR"),this.$={stmt:"dir",value:"LR"};break;case 44:case 45:this.$={stmt:"state",id:n[o].trim(),type:"default",description:""};break;case 46:case 47:this.$={stmt:"state",id:n[o-2].trim(),classes:[n[o].trim()],type:"default",description:""}}},"anonymous"),table:[{3:1,4:e,5:s,6:i},{1:[3]},{3:5,4:e,5:s,6:i},{3:6,4:e,5:s,6:i},t([1,4,5,16,17,19,22,24,25,26,27,28,29,33,35,37,38,41,45,48,51,52,53,54,57],r,{7:7}),{1:[2,1]},{1:[2,2]},{1:[2,3],4:a,5:o,8:8,9:10,10:12,11:13,12:14,13:15,16:l,17:c,19:h,22:d,24:u,25:p,26:g,27:y,28:m,29:f,32:25,33:_,35:S,37:b,38:T,41:k,45:E,48:C,51:D,52:x,53:$,54:v,57:A},t(I,[2,5]),{9:39,10:12,11:13,12:14,13:15,16:l,17:c,19:h,22:d,24:u,25:p,26:g,27:y,28:m,29:f,32:25,33:_,35:S,37:b,38:T,41:k,45:E,48:C,51:D,52:x,53:$,54:v,57:A},t(I,[2,7]),t(I,[2,8]),t(I,[2,9]),t(I,[2,10]),t(I,[2,11]),t(I,[2,12],{14:[1,40],15:[1,41]}),t(I,[2,16]),{18:[1,42]},t(I,[2,18],{20:[1,43]}),{23:[1,44]},t(I,[2,22]),t(I,[2,23]),t(I,[2,24]),t(I,[2,25]),{30:45,31:[1,46],59:[1,47],60:[1,48]},t(I,[2,28]),{34:[1,49]},{36:[1,50]},t(I,[2,31]),{13:51,24:u,57:A},{42:[1,52],44:[1,53]},{46:[1,54]},{49:[1,55]},t(L,[2,44],{58:[1,56]}),t(L,[2,45],{58:[1,57]}),t(I,[2,38]),t(I,[2,39]),t(I,[2,40]),t(I,[2,41]),t(I,[2,6]),t(I,[2,13]),{13:58,24:u,57:A},t(I,[2,17]),t(w,r,{7:59}),{24:[1,60]},{24:[1,61]},{23:[1,62]},{24:[2,48]},{24:[2,49]},t(I,[2,29]),t(I,[2,30]),{39:[1,63],40:[1,64]},{43:[1,65]},{43:[1,66]},{47:[1,67]},{50:[1,68]},{24:[1,69]},{24:[1,70]},t(I,[2,14],{14:[1,71]}),{4:a,5:o,8:8,9:10,10:12,11:13,12:14,13:15,16:l,17:c,19:h,21:[1,72],22:d,24:u,25:p,26:g,27:y,28:m,29:f,32:25,33:_,35:S,37:b,38:T,41:k,45:E,48:C,51:D,52:x,53:$,54:v,57:A},t(I,[2,20],{20:[1,73]}),{31:[1,74]},{24:[1,75]},{39:[1,76]},{39:[1,77]},t(I,[2,34]),t(I,[2,35]),t(I,[2,36]),t(I,[2,37]),t(L,[2,46]),t(L,[2,47]),t(I,[2,15]),t(I,[2,19]),t(w,r,{7:78}),t(I,[2,26]),t(I,[2,27]),{5:[1,79]},{5:[1,80]},{4:a,5:o,8:8,9:10,10:12,11:13,12:14,13:15,16:l,17:c,19:h,21:[1,81],22:d,24:u,25:p,26:g,27:y,28:m,29:f,32:25,33:_,35:S,37:b,38:T,41:k,45:E,48:C,51:D,52:x,53:$,54:v,57:A},t(I,[2,32]),t(I,[2,33]),t(I,[2,21])],defaultActions:{5:[2,1],6:[2,2],47:[2,48],48:[2,49]},parseError:(0,n.__name)(function(t,e){if(e.recoverable)this.trace(t);else{var s=Error(t);throw s.hash=e,s}},"parseError"),parse:(0,n.__name)(function(t){var e=this,s=[0],i=[],r=[null],a=[],o=this.table,l="",c=0,h=0,d=0,u=a.slice.call(arguments,1),p=Object.create(this.lexer),g={};for(var y in this.yy)Object.prototype.hasOwnProperty.call(this.yy,y)&&(g[y]=this.yy[y]);p.setInput(t,g),g.lexer=p,g.parser=this,void 0===p.yylloc&&(p.yylloc={});var m=p.yylloc;a.push(m);var f=p.options&&p.options.ranges;function _(){var t;return"number"!=typeof(t=i.pop()||p.lex()||1)&&(t instanceof Array&&(t=(i=t).pop()),t=e.symbols_[t]||t),t}"function"==typeof g.parseError?this.parseError=g.parseError:this.parseError=Object.getPrototypeOf(this).parseError,(0,n.__name)(function(t){s.length=s.length-2*t,r.length=r.length-t,a.length=a.length-t},"popStack"),(0,n.__name)(_,"lex");for(var S,b,T,k,E,C,D,x,$,v={};;){if(T=s[s.length-1],this.defaultActions[T]?k=this.defaultActions[T]:(null==S&&(S=_()),k=o[T]&&o[T][S]),void 0===k||!k.length||!k[0]){var A="";for(C in $=[],o[T])this.terminals_[C]&&C>2&&$.push("'"+this.terminals_[C]+"'");A=p.showPosition?"Parse error on line "+(c+1)+":\n"+p.showPosition()+"\nExpecting "+$.join(", ")+", got '"+(this.terminals_[S]||S)+"'":"Parse error on line "+(c+1)+": Unexpected "+(1==S?"end of input":"'"+(this.terminals_[S]||S)+"'"),this.parseError(A,{text:p.match,token:this.terminals_[S]||S,line:p.yylineno,loc:m,expected:$})}if(k[0]instanceof Array&&k.length>1)throw Error("Parse Error: multiple actions possible at state: "+T+", token: "+S);switch(k[0]){case 1:s.push(S),r.push(p.yytext),a.push(p.yylloc),s.push(k[1]),S=null,b?(S=b,b=null):(h=p.yyleng,l=p.yytext,c=p.yylineno,m=p.yylloc,d>0&&d--);break;case 2:if(D=this.productions_[k[1]][1],v.$=r[r.length-D],v._$={first_line:a[a.length-(D||1)].first_line,last_line:a[a.length-1].last_line,first_column:a[a.length-(D||1)].first_column,last_column:a[a.length-1].last_column},f&&(v._$.range=[a[a.length-(D||1)].range[0],a[a.length-1].range[1]]),void 0!==(E=this.performAction.apply(v,[l,h,c,g,k[1],r,a].concat(u))))return E;D&&(s=s.slice(0,-1*D*2),r=r.slice(0,-1*D),a=a.slice(0,-1*D)),s.push(this.productions_[k[1]][0]),r.push(v.$),a.push(v._$),x=o[s[s.length-2]][s[s.length-1]],s.push(x);break;case 3:return!0}}return!0},"parse")};function O(){this.yy={}}return N.lexer={EOF:1,parseError:(0,n.__name)(function(t,e){if(this.yy.parser)this.yy.parser.parseError(t,e);else throw Error(t)},"parseError"),setInput:(0,n.__name)(function(t,e){return this.yy=e||this.yy||{},this._input=t,this._more=this._backtrack=this.done=!1,this.yylineno=this.yyleng=0,this.yytext=this.matched=this.match="",this.conditionStack=["INITIAL"],this.yylloc={first_line:1,first_column:0,last_line:1,last_column:0},this.options.ranges&&(this.yylloc.range=[0,0]),this.offset=0,this},"setInput"),input:(0,n.__name)(function(){var t=this._input[0];return this.yytext+=t,this.yyleng++,this.offset++,this.match+=t,this.matched+=t,t.match(/(?:\r\n?|\n).*/g)?(this.yylineno++,this.yylloc.last_line++):this.yylloc.last_column++,this.options.ranges&&this.yylloc.range[1]++,this._input=this._input.slice(1),t},"input"),unput:(0,n.__name)(function(t){var e=t.length,s=t.split(/(?:\r\n?|\n)/g);this._input=t+this._input,this.yytext=this.yytext.substr(0,this.yytext.length-e),this.offset-=e;var i=this.match.split(/(?:\r\n?|\n)/g);this.match=this.match.substr(0,this.match.length-1),this.matched=this.matched.substr(0,this.matched.length-1),s.length-1&&(this.yylineno-=s.length-1);var r=this.yylloc.range;return this.yylloc={first_line:this.yylloc.first_line,last_line:this.yylineno+1,first_column:this.yylloc.first_column,last_column:s?(s.length===i.length?this.yylloc.first_column:0)+i[i.length-s.length].length-s[0].length:this.yylloc.first_column-e},this.options.ranges&&(this.yylloc.range=[r[0],r[0]+this.yyleng-e]),this.yyleng=this.yytext.length,this},"unput"),more:(0,n.__name)(function(){return this._more=!0,this},"more"),reject:(0,n.__name)(function(){return this.options.backtrack_lexer?(this._backtrack=!0,this):this.parseError("Lexical error on line "+(this.yylineno+1)+". You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n"+this.showPosition(),{text:"",token:null,line:this.yylineno})},"reject"),less:(0,n.__name)(function(t){this.unput(this.match.slice(t))},"less"),pastInput:(0,n.__name)(function(){var t=this.matched.substr(0,this.matched.length-this.match.length);return(t.length>20?"...":"")+t.substr(-20).replace(/\n/g,"")},"pastInput"),upcomingInput:(0,n.__name)(function(){var t=this.match;return t.length<20&&(t+=this._input.substr(0,20-t.length)),(t.substr(0,20)+(t.length>20?"...":"")).replace(/\n/g,"")},"upcomingInput"),showPosition:(0,n.__name)(function(){var t=this.pastInput(),e=Array(t.length+1).join("-");return t+this.upcomingInput()+"\n"+e+"^"},"showPosition"),test_match:(0,n.__name)(function(t,e){var s,i,r;if(this.options.backtrack_lexer&&(r={yylineno:this.yylineno,yylloc:{first_line:this.yylloc.first_line,last_line:this.last_line,first_column:this.yylloc.first_column,last_column:this.yylloc.last_column},yytext:this.yytext,match:this.match,matches:this.matches,matched:this.matched,yyleng:this.yyleng,offset:this.offset,_more:this._more,_input:this._input,yy:this.yy,conditionStack:this.conditionStack.slice(0),done:this.done},this.options.ranges&&(r.yylloc.range=this.yylloc.range.slice(0))),(i=t[0].match(/(?:\r\n?|\n).*/g))&&(this.yylineno+=i.length),this.yylloc={first_line:this.yylloc.last_line,last_line:this.yylineno+1,first_column:this.yylloc.last_column,last_column:i?i[i.length-1].length-i[i.length-1].match(/\r?\n?/)[0].length:this.yylloc.last_column+t[0].length},this.yytext+=t[0],this.match+=t[0],this.matches=t,this.yyleng=this.yytext.length,this.options.ranges&&(this.yylloc.range=[this.offset,this.offset+=this.yyleng]),this._more=!1,this._backtrack=!1,this._input=this._input.slice(t[0].length),this.matched+=t[0],s=this.performAction.call(this,this.yy,this,e,this.conditionStack[this.conditionStack.length-1]),this.done&&this._input&&(this.done=!1),s)return s;if(this._backtrack)for(var n in r)this[n]=r[n];return!1},"test_match"),next:(0,n.__name)(function(){if(this.done)return this.EOF;this._input||(this.done=!0),this._more||(this.yytext="",this.match="");for(var t,e,s,i,r=this._currentRules(),n=0;n<r.length;n++)if((s=this._input.match(this.rules[r[n]]))&&(!e||s[0].length>e[0].length)){if(e=s,i=n,this.options.backtrack_lexer){if(!1!==(t=this.test_match(s,r[n])))return t;if(!this._backtrack)return!1;e=!1;continue}if(!this.options.flex)break}return e?!1!==(t=this.test_match(e,r[i]))&&t:""===this._input?this.EOF:this.parseError("Lexical error on line "+(this.yylineno+1)+". Unrecognized text.\n"+this.showPosition(),{text:"",token:null,line:this.yylineno})},"next"),lex:(0,n.__name)(function(){var t=this.next();return t||this.lex()},"lex"),begin:(0,n.__name)(function(t){this.conditionStack.push(t)},"begin"),popState:(0,n.__name)(function(){return this.conditionStack.length-1>0?this.conditionStack.pop():this.conditionStack[0]},"popState"),_currentRules:(0,n.__name)(function(){return this.conditionStack.length&&this.conditionStack[this.conditionStack.length-1]?this.conditions[this.conditionStack[this.conditionStack.length-1]].rules:this.conditions.INITIAL.rules},"_currentRules"),topState:(0,n.__name)(function(t){return(t=this.conditionStack.length-1-Math.abs(t||0))>=0?this.conditionStack[t]:"INITIAL"},"topState"),pushState:(0,n.__name)(function(t){this.begin(t)},"pushState"),stateStackSize:(0,n.__name)(function(){return this.conditionStack.length},"stateStackSize"),options:{"case-insensitive":!0},performAction:(0,n.__name)(function(t,e,s,i){switch(s){case 0:return 38;case 1:return 40;case 2:return 39;case 3:return 44;case 4:case 45:return 51;case 5:case 46:return 52;case 6:case 47:return 53;case 7:case 48:return 54;case 8:case 9:case 11:case 12:case 13:case 14:case 57:case 59:case 65:break;case 10:case 80:return 5;case 15:case 35:return this.pushState("SCALE"),17;case 16:case 36:return 18;case 17:case 23:case 37:case 52:case 55:this.popState();break;case 18:return this.begin("acc_title"),33;case 19:return this.popState(),"acc_title_value";case 20:return this.begin("acc_descr"),35;case 21:return this.popState(),"acc_descr_value";case 22:this.begin("acc_descr_multiline");break;case 24:return"acc_descr_multiline_value";case 25:return this.pushState("CLASSDEF"),41;case 26:return this.popState(),this.pushState("CLASSDEFID"),"DEFAULT_CLASSDEF_ID";case 27:return this.popState(),this.pushState("CLASSDEFID"),42;case 28:return this.popState(),43;case 29:return this.pushState("CLASS"),48;case 30:return this.popState(),this.pushState("CLASS_STYLE"),49;case 31:return this.popState(),50;case 32:return this.pushState("STYLE"),45;case 33:return this.popState(),this.pushState("STYLEDEF_STYLES"),46;case 34:return this.popState(),47;case 38:this.pushState("STATE");break;case 39:case 42:return this.popState(),e.yytext=e.yytext.slice(0,-8).trim(),25;case 40:case 43:return this.popState(),e.yytext=e.yytext.slice(0,-8).trim(),26;case 41:case 44:return this.popState(),e.yytext=e.yytext.slice(0,-10).trim(),27;case 49:this.pushState("STATE_STRING");break;case 50:return this.pushState("STATE_ID"),"AS";case 51:case 67:return this.popState(),"ID";case 53:return"STATE_DESCR";case 54:return 19;case 56:return this.popState(),this.pushState("struct"),20;case 58:return this.popState(),21;case 60:return this.begin("NOTE"),29;case 61:return this.popState(),this.pushState("NOTE_ID"),59;case 62:return this.popState(),this.pushState("NOTE_ID"),60;case 63:this.popState(),this.pushState("FLOATING_NOTE");break;case 64:return this.popState(),this.pushState("FLOATING_NOTE_ID"),"AS";case 66:return"NOTE_TEXT";case 68:return this.popState(),this.pushState("NOTE_TEXT"),24;case 69:return this.popState(),e.yytext=e.yytext.substr(2).trim(),31;case 70:return this.popState(),e.yytext=e.yytext.slice(0,-8).trim(),31;case 71:case 72:return 6;case 73:return 16;case 74:return 57;case 75:return 24;case 76:return e.yytext=e.yytext.trim(),14;case 77:return 15;case 78:return 28;case 79:return 58;case 81:return"INVALID"}},"anonymous"),rules:[/^(?:click\b)/i,/^(?:href\b)/i,/^(?:"[^"]*")/i,/^(?:default\b)/i,/^(?:.*direction\s+TB[^\n]*)/i,/^(?:.*direction\s+BT[^\n]*)/i,/^(?:.*direction\s+RL[^\n]*)/i,/^(?:.*direction\s+LR[^\n]*)/i,/^(?:%%(?!\{)[^\n]*)/i,/^(?:[^\}]%%[^\n]*)/i,/^(?:[\n]+)/i,/^(?:[\s]+)/i,/^(?:((?!\n)\s)+)/i,/^(?:#[^\n]*)/i,/^(?:%[^\n]*)/i,/^(?:scale\s+)/i,/^(?:\d+)/i,/^(?:\s+width\b)/i,/^(?:accTitle\s*:\s*)/i,/^(?:(?!\n||)*[^\n]*)/i,/^(?:accDescr\s*:\s*)/i,/^(?:(?!\n||)*[^\n]*)/i,/^(?:accDescr\s*\{\s*)/i,/^(?:[\}])/i,/^(?:[^\}]*)/i,/^(?:classDef\s+)/i,/^(?:DEFAULT\s+)/i,/^(?:\w+\s+)/i,/^(?:[^\n]*)/i,/^(?:class\s+)/i,/^(?:(\w+)+((,\s*\w+)*))/i,/^(?:[^\n]*)/i,/^(?:style\s+)/i,/^(?:[\w,]+\s+)/i,/^(?:[^\n]*)/i,/^(?:scale\s+)/i,/^(?:\d+)/i,/^(?:\s+width\b)/i,/^(?:state\s+)/i,/^(?:.*<<fork>>)/i,/^(?:.*<<join>>)/i,/^(?:.*<<choice>>)/i,/^(?:.*\[\[fork\]\])/i,/^(?:.*\[\[join\]\])/i,/^(?:.*\[\[choice\]\])/i,/^(?:.*direction\s+TB[^\n]*)/i,/^(?:.*direction\s+BT[^\n]*)/i,/^(?:.*direction\s+RL[^\n]*)/i,/^(?:.*direction\s+LR[^\n]*)/i,/^(?:["])/i,/^(?:\s*as\s+)/i,/^(?:[^\n\{]*)/i,/^(?:["])/i,/^(?:[^"]*)/i,/^(?:[^\n\s\{]+)/i,/^(?:\n)/i,/^(?:\{)/i,/^(?:%%(?!\{)[^\n]*)/i,/^(?:\})/i,/^(?:[\n])/i,/^(?:note\s+)/i,/^(?:left of\b)/i,/^(?:right of\b)/i,/^(?:")/i,/^(?:\s*as\s*)/i,/^(?:["])/i,/^(?:[^"]*)/i,/^(?:[^\n]*)/i,/^(?:\s*[^:\n\s\-]+)/i,/^(?:\s*:[^:\n;]+)/i,/^(?:[\s\S]*?end note\b)/i,/^(?:stateDiagram\s+)/i,/^(?:stateDiagram-v2\s+)/i,/^(?:hide empty description\b)/i,/^(?:\[\*\])/i,/^(?:[^:\n\s\-\{]+)/i,/^(?:\s*:[^:\n;]+)/i,/^(?:-->)/i,/^(?:--)/i,/^(?::::)/i,/^(?:$)/i,/^(?:.)/i],conditions:{LINE:{rules:[12,13],inclusive:!1},struct:{rules:[12,13,25,29,32,38,45,46,47,48,57,58,59,60,74,75,76,77,78],inclusive:!1},FLOATING_NOTE_ID:{rules:[67],inclusive:!1},FLOATING_NOTE:{rules:[64,65,66],inclusive:!1},NOTE_TEXT:{rules:[69,70],inclusive:!1},NOTE_ID:{rules:[68],inclusive:!1},NOTE:{rules:[61,62,63],inclusive:!1},STYLEDEF_STYLEOPTS:{rules:[],inclusive:!1},STYLEDEF_STYLES:{rules:[34],inclusive:!1},STYLE_IDS:{rules:[],inclusive:!1},STYLE:{rules:[33],inclusive:!1},CLASS_STYLE:{rules:[31],inclusive:!1},CLASS:{rules:[30],inclusive:!1},CLASSDEFID:{rules:[28],inclusive:!1},CLASSDEF:{rules:[26,27],inclusive:!1},acc_descr_multiline:{rules:[23,24],inclusive:!1},acc_descr:{rules:[21],inclusive:!1},acc_title:{rules:[19],inclusive:!1},SCALE:{rules:[16,17,36,37],inclusive:!1},ALIAS:{rules:[],inclusive:!1},STATE_ID:{rules:[51],inclusive:!1},STATE_STRING:{rules:[52,53],inclusive:!1},FORK_STATE:{rules:[],inclusive:!1},STATE:{rules:[12,13,39,40,41,42,43,44,49,50,54,55,56],inclusive:!1},ID:{rules:[12,13],inclusive:!1},INITIAL:{rules:[0,1,2,3,4,5,6,7,8,9,10,11,13,14,15,18,20,22,25,29,32,35,38,56,60,71,72,73,74,75,76,77,79,80,81],inclusive:!0}}},(0,n.__name)(O,"Parser"),O.prototype=N,N.Parser=O,new O}();a.parser=a;var o="TB",l="state",c="root",h="relation",d="default",u="divider",p="fill:none",g="fill: #333",y="text",m="normal",f="rect",_="rectWithTitle",S="divider",b="roundedWithTitle",T="statediagram",k=`${T}-state`,E="transition",C=`${E} note-edge`,D=`${T}-note`,x=`${T}-cluster`,$=`${T}-cluster-alt`,v="parent",A="note",I="----",L=`${I}${A}`,w=`${I}${v}`,N=(0,n.__name)((t,e=o)=>{if(!t.doc)return e;let s=e;for(let e of t.doc)"dir"===e.stmt&&(s=e.value);return s},"getDir"),O={getClasses:(0,n.__name)(function(t,e){return e.db.getClasses()},"getClasses"),draw:(0,n.__name)(async function(t,a,o,l){n.log.info("REF0:"),n.log.info("Drawing state diagram (v2)",a);let{securityLevel:c,state:h,layout:d}=(0,n.getConfig2)();l.db.extract(l.db.getRootDocV2());let u=l.db.getData(),p=(0,e.getDiagramElement)(a,c);u.type=l.type,u.layoutAlgorithm=d,u.nodeSpacing=h?.nodeSpacing||50,u.rankSpacing=h?.rankSpacing||50,u.markers=["barb"],u.diagramId=a,await (0,i.render)(u,p);try{("function"==typeof l.db.getLinks?l.db.getLinks():new Map).forEach((t,e)=>{let s,i="string"==typeof e?e:"string"==typeof e?.id?e.id:"";if(!i)return void n.log.warn("⚠️ Invalid or missing stateId from key:",JSON.stringify(e));let r=p.node()?.querySelectorAll("g");if(r?.forEach(t=>{t.textContent?.trim()===i&&(s=t)}),!s)return void n.log.warn("⚠️ Could not find node matching text:",i);let a=s.parentNode;if(!a)return void n.log.warn("⚠️ Node has no parent, cannot wrap:",i);let o=document.createElementNS("http://www.w3.org/2000/svg","a"),l=t.url.replace(/^"+|"+$/g,"");if(o.setAttributeNS("http://www.w3.org/1999/xlink","xlink:href",l),o.setAttribute("target","_blank"),t.tooltip){let e=t.tooltip.replace(/^"+|"+$/g,"");o.setAttribute("title",e)}a.replaceChild(o,s),o.appendChild(s),n.log.info("🔗 Wrapped node in <a> tag for:",i,t.url)})}catch(t){n.log.error("❌ Error injecting clickable links:",t)}r.utils_default.insertTitle(p,"statediagramTitleText",h?.titleTopMargin??25,l.db.getDiagramTitle()),(0,s.setupViewPortForSVG)(p,8,T,h?.useMaxWidth??!0)},"draw"),getDir:N},R=new Map,B=0;function F(t="",e=0,s="",i=I){let r=null!==s&&s.length>0?`${i}${s}`:"";return`state-${t}${r}-${e}`}(0,n.__name)(F,"stateDomId");var P=(0,n.__name)((t,e,s,i,r,a,o,c)=>{n.log.trace("items",e),e.forEach(e=>{switch(e.stmt){case l:case d:M(t,e,s,i,r,a,o,c);break;case h:{M(t,e.state1,s,i,r,a,o,c),M(t,e.state2,s,i,r,a,o,c);let l={id:"edge"+B,start:e.state1.id,end:e.state2.id,arrowhead:"normal",arrowTypeEnd:"arrow_barb",style:p,labelStyle:"",label:n.common_default.sanitizeText(e.description??"",(0,n.getConfig2)()),arrowheadStyle:g,labelpos:"c",labelType:y,thickness:m,classes:E,look:o};r.push(l),B++}}})},"setupDoc"),G=(0,n.__name)((t,e=o)=>{let s=e;if(t.doc)for(let e of t.doc)"dir"===e.stmt&&(s=e.value);return s},"getDir");function Y(t,e,s){if(!e.id||"</join></fork>"===e.id||"</choice>"===e.id)return;e.cssClasses&&(Array.isArray(e.cssCompiledStyles)||(e.cssCompiledStyles=[]),e.cssClasses.split(" ").forEach(t=>{let i=s.get(t);i&&(e.cssCompiledStyles=[...e.cssCompiledStyles??[],...i.styles])}));let i=t.find(t=>t.id===e.id);i?Object.assign(i,e):t.push(e)}function j(t){return t?.classes?.join(" ")??""}function z(t){return t?.styles??[]}(0,n.__name)(Y,"insertOrUpdateNode"),(0,n.__name)(j,"getClassesFromDbInfo"),(0,n.__name)(z,"getStylesFromDbInfo");var M=(0,n.__name)((t,e,s,i,r,a,o,l)=>{let c=e.id,h=s.get(c),T=j(h),E=z(h),I=(0,n.getConfig2)();if(n.log.info("dataFetcher parsedItem",e,h,E),"root"!==c){let s=f;!0===e.start?s="stateStart":!1===e.start&&(s="stateEnd"),e.type!==d&&(s=e.type),R.get(c)||R.set(c,{id:c,shape:s,description:n.common_default.sanitizeText(c,I),cssClasses:`${T} ${k}`,cssStyles:E});let h=R.get(c);e.description&&(Array.isArray(h.description)?(h.shape=_,h.description.push(e.description)):h.description?.length&&h.description.length>0?(h.shape=_,h.description===c?h.description=[e.description]:h.description=[h.description,e.description]):(h.shape=f,h.description=e.description),h.description=n.common_default.sanitizeTextOrArray(h.description,I)),h.description?.length===1&&h.shape===_&&("group"===h.type?h.shape=b:h.shape=f),!h.type&&e.doc&&(n.log.info("Setting cluster for XCX",c,G(e)),h.type="group",h.isGroup=!0,h.dir=G(e),h.shape=e.type===u?S:b,h.cssClasses=`${h.cssClasses} ${x} ${a?$:""}`);let N={labelStyle:"",shape:h.shape,label:h.description,cssClasses:h.cssClasses,cssCompiledStyles:[],cssStyles:h.cssStyles,id:c,dir:h.dir,domId:F(c,B),type:h.type,isGroup:"group"===h.type,padding:8,rx:10,ry:10,look:o};if(N.shape===S&&(N.label=""),t&&"root"!==t.id&&(n.log.trace("Setting node ",c," to be child of its parent ",t.id),N.parentId=t.id),N.centerLabel=!0,e.note){let t={labelStyle:"",shape:"note",label:e.note.text,cssClasses:D,cssStyles:[],cssCompiledStyles:[],id:c+L+"-"+B,domId:F(c,B,A),type:h.type,isGroup:"group"===h.type,padding:I.flowchart?.padding,look:o,position:e.note.position},s=c+w,n={labelStyle:"",shape:"noteGroup",label:e.note.text,cssClasses:h.cssClasses,cssStyles:[],id:c+w,domId:F(c,B,v),type:"group",isGroup:!0,padding:16,look:o,position:e.note.position};B++,n.id=s,t.parentId=s,Y(i,n,l),Y(i,t,l),Y(i,N,l);let a=c,d=t.id;"left of"===e.note.position&&(a=t.id,d=c),r.push({id:a+"-"+d,start:a,end:d,arrowhead:"none",arrowTypeEnd:"",style:p,labelStyle:"",classes:C,arrowheadStyle:g,labelpos:"c",labelType:y,thickness:m,look:o})}else Y(i,N,l)}e.doc&&(n.log.trace("Adding nodes children "),P(e,e.doc,s,i,r,!a,o,l))},"dataFetcher"),U=(0,n.__name)(()=>{R.clear(),B=0},"reset"),V="start",W="color",H="fill",X=(0,n.__name)(()=>new Map,"newClassesList"),K=(0,n.__name)(()=>({relations:[],states:new Map,documents:{}}),"newDoc"),J=(0,n.__name)(t=>JSON.parse(JSON.stringify(t)),"clone"),q=class{constructor(t){this.version=t,this.nodes=[],this.edges=[],this.rootDoc=[],this.classes=X(),this.documents={root:K()},this.currentDocument=this.documents.root,this.startEndCount=0,this.dividerCnt=0,this.links=new Map,this.getAccTitle=n.getAccTitle,this.setAccTitle=n.setAccTitle,this.getAccDescription=n.getAccDescription,this.setAccDescription=n.setAccDescription,this.setDiagramTitle=n.setDiagramTitle,this.getDiagramTitle=n.getDiagramTitle,this.clear(),this.setRootDoc=this.setRootDoc.bind(this),this.getDividerId=this.getDividerId.bind(this),this.setDirection=this.setDirection.bind(this),this.trimColon=this.trimColon.bind(this)}static{(0,n.__name)(this,"StateDB")}static{this.relationType={AGGREGATION:0,EXTENSION:1,COMPOSITION:2,DEPENDENCY:3}}extract(t){for(let e of(this.clear(!0),Array.isArray(t)?t:t.doc))switch(e.stmt){case l:this.addState(e.id.trim(),e.type,e.doc,e.description,e.note);break;case h:this.addRelation(e.state1,e.state2,e.description);break;case"classDef":this.addStyleClass(e.id.trim(),e.classes);break;case"style":this.handleStyleDef(e);break;case"applyClass":this.setCssClass(e.id.trim(),e.styleClass);break;case"click":this.addLink(e.id,e.url,e.tooltip)}let e=this.getStates(),s=(0,n.getConfig2)();for(let t of(U(),M(void 0,this.getRootDocV2(),e,this.nodes,this.edges,!0,s.look,this.classes),this.nodes))if(Array.isArray(t.label)){if(t.description=t.label.slice(1),t.isGroup&&t.description.length>0)throw Error(`Group nodes can only have label. Remove the additional description for node [${t.id}]`);t.label=t.label[0]}}handleStyleDef(t){let e=t.id.trim().split(","),s=t.styleClass.split(",");for(let t of e){let e=this.getState(t);if(!e){let s=t.trim();this.addState(s),e=this.getState(s)}e&&(e.styles=s.map(t=>t.replace(/;/g,"")?.trim()))}}setRootDoc(t){n.log.info("Setting root doc",t),this.rootDoc=t,1===this.version?this.extract(t):this.extract(this.getRootDocV2())}docTranslator(t,e,s){if(e.stmt===h){this.docTranslator(t,e.state1,!0),this.docTranslator(t,e.state2,!1);return}if(e.stmt===l&&("[*]"===e.id?(e.id=t.id+(s?"_start":"_end"),e.start=s):e.id=e.id.trim()),e.stmt!==c&&e.stmt!==l||!e.doc)return;let i=[],n=[];for(let t of e.doc)if(t.type===u){let e=J(t);e.doc=J(n),i.push(e),n=[]}else n.push(t);if(i.length>0&&n.length>0){let t={stmt:l,id:(0,r.generateId)(),type:"divider",doc:J(n)};i.push(J(t)),e.doc=i}e.doc.forEach(t=>this.docTranslator(e,t,!0))}getRootDocV2(){return this.docTranslator({id:c,stmt:c},{id:c,stmt:c,doc:this.rootDoc},!0),{id:c,doc:this.rootDoc}}addState(t,e=d,s,i,r,a,o,c){let h=t?.trim();if(this.currentDocument.states.has(h)){let t=this.currentDocument.states.get(h);if(!t)throw Error(`State not found: ${h}`);t.doc||(t.doc=s),t.type||(t.type=e)}else n.log.info("Adding state ",h,i),this.currentDocument.states.set(h,{stmt:l,id:h,descriptions:[],type:e,doc:s,note:r,classes:[],styles:[],textStyles:[]});if(i&&(n.log.info("Setting state description",h,i),(Array.isArray(i)?i:[i]).forEach(t=>this.addDescription(h,t.trim()))),r){let t=this.currentDocument.states.get(h);if(!t)throw Error(`State not found: ${h}`);t.note=r,t.note.text=n.common_default.sanitizeText(t.note.text,(0,n.getConfig2)())}a&&(n.log.info("Setting state classes",h,a),(Array.isArray(a)?a:[a]).forEach(t=>this.setCssClass(h,t.trim()))),o&&(n.log.info("Setting state styles",h,o),(Array.isArray(o)?o:[o]).forEach(t=>this.setStyle(h,t.trim()))),c&&(n.log.info("Setting state styles",h,o),(Array.isArray(c)?c:[c]).forEach(t=>this.setTextStyle(h,t.trim())))}clear(t){this.nodes=[],this.edges=[],this.documents={root:K()},this.currentDocument=this.documents.root,this.startEndCount=0,this.classes=X(),t||(this.links=new Map,(0,n.clear)())}getState(t){return this.currentDocument.states.get(t)}getStates(){return this.currentDocument.states}logDocuments(){n.log.info("Documents = ",this.documents)}getRelations(){return this.currentDocument.relations}addLink(t,e,s){this.links.set(t,{url:e,tooltip:s}),n.log.warn("Adding link",t,e,s)}getLinks(){return this.links}startIdIfNeeded(t=""){return"[*]"===t?(this.startEndCount++,`${V}${this.startEndCount}`):t}startTypeIfNeeded(t="",e=d){return"[*]"===t?V:e}endIdIfNeeded(t=""){return"[*]"===t?(this.startEndCount++,`end${this.startEndCount}`):t}endTypeIfNeeded(t="",e=d){return"[*]"===t?"end":e}addRelationObjs(t,e,s=""){let i=this.startIdIfNeeded(t.id.trim()),r=this.startTypeIfNeeded(t.id.trim(),t.type),a=this.startIdIfNeeded(e.id.trim()),o=this.startTypeIfNeeded(e.id.trim(),e.type);this.addState(i,r,t.doc,t.description,t.note,t.classes,t.styles,t.textStyles),this.addState(a,o,e.doc,e.description,e.note,e.classes,e.styles,e.textStyles),this.currentDocument.relations.push({id1:i,id2:a,relationTitle:n.common_default.sanitizeText(s,(0,n.getConfig2)())})}addRelation(t,e,s){if("object"==typeof t&&"object"==typeof e)this.addRelationObjs(t,e,s);else if("string"==typeof t&&"string"==typeof e){let i=this.startIdIfNeeded(t.trim()),r=this.startTypeIfNeeded(t),a=this.endIdIfNeeded(e.trim()),o=this.endTypeIfNeeded(e);this.addState(i,r),this.addState(a,o),this.currentDocument.relations.push({id1:i,id2:a,relationTitle:s?n.common_default.sanitizeText(s,(0,n.getConfig2)()):void 0})}}addDescription(t,e){let s=this.currentDocument.states.get(t),i=e.startsWith(":")?e.replace(":","").trim():e;s?.descriptions?.push(n.common_default.sanitizeText(i,(0,n.getConfig2)()))}cleanupLabel(t){return t.startsWith(":")?t.slice(2).trim():t.trim()}getDividerId(){return this.dividerCnt++,`divider-id-${this.dividerCnt}`}addStyleClass(t,e=""){this.classes.has(t)||this.classes.set(t,{id:t,styles:[],textStyles:[]});let s=this.classes.get(t);e&&s&&e.split(",").forEach(t=>{let e=t.replace(/([^;]*);/,"$1").trim();if(RegExp(W).exec(t)){let t=e.replace(H,"bgFill").replace(W,H);s.textStyles.push(t)}s.styles.push(e)})}getClasses(){return this.classes}setCssClass(t,e){t.split(",").forEach(t=>{let s=this.getState(t);if(!s){let e=t.trim();this.addState(e),s=this.getState(e)}s?.classes?.push(e)})}setStyle(t,e){this.getState(t)?.styles?.push(e)}setTextStyle(t,e){this.getState(t)?.textStyles?.push(e)}getDirectionStatement(){return this.rootDoc.find(t=>"dir"===t.stmt)}getDirection(){return this.getDirectionStatement()?.value??"TB"}setDirection(t){let e=this.getDirectionStatement();e?e.value=t:this.rootDoc.unshift({stmt:"dir",value:t})}trimColon(t){return t.startsWith(":")?t.slice(1).trim():t.trim()}getData(){let t=(0,n.getConfig2)();return{nodes:this.nodes,edges:this.edges,other:{},config:t,direction:N(this.getRootDocV2())}}getConfig(){return(0,n.getConfig2)().state}},Q=(0,n.__name)(t=>`
defs #statediagram-barbEnd {
    fill: ${t.transitionColor};
    stroke: ${t.transitionColor};
  }
g.stateGroup text {
  fill: ${t.nodeBorder};
  stroke: none;
  font-size: 10px;
}
g.stateGroup text {
  fill: ${t.textColor};
  stroke: none;
  font-size: 10px;

}
g.stateGroup .state-title {
  font-weight: bolder;
  fill: ${t.stateLabelColor};
}

g.stateGroup rect {
  fill: ${t.mainBkg};
  stroke: ${t.nodeBorder};
}

g.stateGroup line {
  stroke: ${t.lineColor};
  stroke-width: 1;
}

.transition {
  stroke: ${t.transitionColor};
  stroke-width: 1;
  fill: none;
}

.stateGroup .composit {
  fill: ${t.background};
  border-bottom: 1px
}

.stateGroup .alt-composit {
  fill: #e0e0e0;
  border-bottom: 1px
}

.state-note {
  stroke: ${t.noteBorderColor};
  fill: ${t.noteBkgColor};

  text {
    fill: ${t.noteTextColor};
    stroke: none;
    font-size: 10px;
  }
}

.stateLabel .box {
  stroke: none;
  stroke-width: 0;
  fill: ${t.mainBkg};
  opacity: 0.5;
}

.edgeLabel .label rect {
  fill: ${t.labelBackgroundColor};
  opacity: 0.5;
}
.edgeLabel {
  background-color: ${t.edgeLabelBackground};
  p {
    background-color: ${t.edgeLabelBackground};
  }
  rect {
    opacity: 0.5;
    background-color: ${t.edgeLabelBackground};
    fill: ${t.edgeLabelBackground};
  }
  text-align: center;
}
.edgeLabel .label text {
  fill: ${t.transitionLabelColor||t.tertiaryTextColor};
}
.label div .edgeLabel {
  color: ${t.transitionLabelColor||t.tertiaryTextColor};
}

.stateLabel text {
  fill: ${t.stateLabelColor};
  font-size: 10px;
  font-weight: bold;
}

.node circle.state-start {
  fill: ${t.specialStateColor};
  stroke: ${t.specialStateColor};
}

.node .fork-join {
  fill: ${t.specialStateColor};
  stroke: ${t.specialStateColor};
}

.node circle.state-end {
  fill: ${t.innerEndBackground};
  stroke: ${t.background};
  stroke-width: 1.5
}
.end-state-inner {
  fill: ${t.compositeBackground||t.background};
  // stroke: ${t.background};
  stroke-width: 1.5
}

.node rect {
  fill: ${t.stateBkg||t.mainBkg};
  stroke: ${t.stateBorder||t.nodeBorder};
  stroke-width: 1px;
}
.node polygon {
  fill: ${t.mainBkg};
  stroke: ${t.stateBorder||t.nodeBorder};;
  stroke-width: 1px;
}
#statediagram-barbEnd {
  fill: ${t.lineColor};
}

.statediagram-cluster rect {
  fill: ${t.compositeTitleBackground};
  stroke: ${t.stateBorder||t.nodeBorder};
  stroke-width: 1px;
}

.cluster-label, .nodeLabel {
  color: ${t.stateLabelColor};
  // line-height: 1;
}

.statediagram-cluster rect.outer {
  rx: 5px;
  ry: 5px;
}
.statediagram-state .divider {
  stroke: ${t.stateBorder||t.nodeBorder};
}

.statediagram-state .title-state {
  rx: 5px;
  ry: 5px;
}
.statediagram-cluster.statediagram-cluster .inner {
  fill: ${t.compositeBackground||t.background};
}
.statediagram-cluster.statediagram-cluster-alt .inner {
  fill: ${t.altBackground?t.altBackground:"#efefef"};
}

.statediagram-cluster .inner {
  rx:0;
  ry:0;
}

.statediagram-state rect.basic {
  rx: 5px;
  ry: 5px;
}
.statediagram-state rect.divider {
  stroke-dasharray: 10,10;
  fill: ${t.altBackground?t.altBackground:"#efefef"};
}

.note-edge {
  stroke-dasharray: 5;
}

.statediagram-note rect {
  fill: ${t.noteBkgColor};
  stroke: ${t.noteBorderColor};
  stroke-width: 1px;
  rx: 0;
  ry: 0;
}
.statediagram-note rect {
  fill: ${t.noteBkgColor};
  stroke: ${t.noteBorderColor};
  stroke-width: 1px;
  rx: 0;
  ry: 0;
}

.statediagram-note text {
  fill: ${t.noteTextColor};
}

.statediagram-note .nodeLabel {
  color: ${t.noteTextColor};
}
.statediagram .edgeLabel {
  color: red; // ${t.noteTextColor};
}

#dependencyStart, #dependencyEnd {
  fill: ${t.lineColor};
  stroke: ${t.lineColor};
  stroke-width: 1;
}

.statediagramTitleText {
  text-anchor: middle;
  font-size: 18px;
  fill: ${t.textColor};
}
`,"getStyles");t.s(["StateDB",()=>q,"stateDiagram_default",()=>a,"stateRenderer_v3_unified_default",()=>O,"styles_default",()=>Q])},47043,t=>{"use strict";var e=t.i(17653);t.i(25628),t.i(50604),t.i(42135),t.i(7276),t.i(61744),t.i(41033),t.i(1884),t.i(10475),t.i(56567),t.i(18873);var s=t.i(44366),i={parser:e.stateDiagram_default,get db(){return new e.StateDB(2)},renderer:e.stateRenderer_v3_unified_default,styles:e.styles_default,init:(0,s.__name)(t=>{t.state||(t.state={}),t.state.arrowMarkerAbsolute=t.arrowMarkerAbsolute},"init")};t.s(["diagram",()=>i])}]);