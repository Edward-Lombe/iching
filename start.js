const http = require('http')
const child_process = require('child_process')
const chokidar = require('chokidar')
const livereload = require('livereload')
const { join } = require('path')
const { Server } = require('node-static')

const ELM_MAIN = join(__dirname, 'Main.elm')
const ELM_JS_OUTPUT = join(__dirname, 'elm.js')
const PORT = 3000

const fileServer = new Server(__dirname)
const server = http
  .createServer((request, response) => {
    request
      .addListener('end', () => {
        fileServer.serve(request, response)
      })
      .resume()
  })
  .listen(PORT)

console.log(`Serving files on ${PORT}`)

const livereloadServer = livereload
  .createServer()
  .watch(ELM_JS_OUTPUT)

const watcher = chokidar
  .watch(ELM_MAIN)
  .on('change', changeHandler)

changeHandler()

function changeHandler() {
  console.log('Building Elm code')
  child_process.exec(`elm make ${ELM_MAIN} --output ${ELM_JS_OUTPUT}`, buildHandler)
}

function buildHandler(error, stdout, stderr) {
  if (error) {
    return
  }
  console.log(stdout.toString())
}