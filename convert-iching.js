const { writeFileSync } = require('fs')
const iChing = require('./iching.json')

const USE_BOOLEANS = true

const converted =
    iChing.hexagrams.hexagram.map(convertPattern)

writeFileSync('./iching-simple.json', JSON.stringify(converted, null, 2))

function convertPattern({pattern}) {
  const convert = letter => {
    const unbroken = letter === "9"
    if (USE_BOOLEANS) {
      return unbroken
    } else {
      return unbroken ? 1 : 0
    }
  }
  return [...pattern].map(convert)
}