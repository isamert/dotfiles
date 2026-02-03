/**
 * Useful for anonymizing some JSON. I generally use this before
 * sending something to an LLM.
 */
function anonymize(obj) {
    if (Array.isArray(obj)) {
        return arr.map((x) => anonymize(x));
    } else if (obj === null) {
        return null;
    } else if (typeof obj === "object") {
        const result = {};
        for (const key in obj) {
            result[key] = anonymize(obj[key]);
        }
        return result;
    }
    if (typeof obj === "string") return "string";
    if (typeof obj === "number") return 1;
    if (typeof obj === "boolean") return true;
    return null;
}
