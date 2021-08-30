emcmake cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug
emmake cmake --build build
python3 -m http.server --directory ./build.fek
