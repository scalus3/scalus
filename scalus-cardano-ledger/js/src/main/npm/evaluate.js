import { Scalus } from "./scalus.js";

const script = "545301010023357389210753756363657373004981";
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 }));
const result = Scalus.evaluateScript(applied);
console.log(result);
console.log(result.budget.memory);
