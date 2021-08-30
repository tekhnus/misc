cmake -S . -B embuild -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug
cmake --build embuild
./build/fek/fek

emcmake cmake -S . -B embuild -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug
emmake cmake --build embuild
python3 -m http.server --directory ./embuild/fek
