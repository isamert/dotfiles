/**
 * Useful for anonymizing some JSON. I generally use this before
 * sending something to an LLM.
 */
function anonymize(obj) {
  if (Array.isArray(obj)) {
    return [];
  } else if (obj === null) {
    return null;
  } else if (typeof obj === "object") {
    const result = {};
    for (const key in obj) {
      const val = obj[key];
      if (typeof val === "string") result[key] = "string";
      else if (typeof val === "number") result[key] = 1;
      else if (typeof val === "boolean") result[key] = true;
      else if (Array.isArray(val)) result[key] = val.map(it => anonymize(it));
      else if (typeof val === "object" && val !== null) result[key] = anonymize(val);
      else result[key] = null;
    }
    return result;
  }
  // Primitive case: default
  if (typeof obj === "string") return "string";
  if (typeof obj === "number") return 1;
  if (typeof obj === "boolean") return true;
  return null;
}
