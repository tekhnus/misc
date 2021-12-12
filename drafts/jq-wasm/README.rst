To build, run `./build.sh` (this requies Docker and nothing else).
It will create a `build` directory with a `jq.js` file in it.

To run the Node.js example: `node examples/from-node.js`.

To run the in-browser example, start your favourite HTTP
server, for example, `python3 -m http.server`,
then open the `examples/from-browser.html`.
The output can be found in the javascript console.
