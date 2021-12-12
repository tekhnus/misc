docker build . -t jq-emscripten
mkdir -p build
docker run --rm -it --mount "type=bind,source=$PWD/build,destination=/build" jq-emscripten cp jq.js /build
