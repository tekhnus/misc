#include <SDL.h>
#include <cstdlib>
#include <exception>
#include <iostream>

using namespace std;

template <typename T> T *sdl_ensure_not_null(T *ptr) {
  if (!ptr) {
    throw runtime_error(SDL_GetError());
  }
  return ptr;
}

class SDL {
public:
  SDL(Uint32 flags) {
    if (SDL_Init(flags)) {
      throw runtime_error(SDL_GetError());
    }
  }

  ~SDL() { SDL_Quit(); }
};

class Window {
public:
  Window(std::string t, int x, int y, int w, int h, Uint32 f)
      : value{sdl_ensure_not_null(SDL_CreateWindow(t.c_str(), x, y, w, h, f)),
              SDL_DestroyWindow} {}

  SDL_Window *get() { return value.get(); }

private:
  unique_ptr<SDL_Window, decltype(&SDL_DestroyWindow)> value;
};

class Renderer {
public:
  Renderer(Window &w, int x, Uint32 f)
      : value{sdl_ensure_not_null(SDL_CreateRenderer(w.get(), x, f)),
              SDL_DestroyRenderer} {}

  SDL_Renderer *get() { return value.get(); }

private:
  unique_ptr<SDL_Renderer, decltype(&SDL_DestroyRenderer)> value;
};

class Surface {
public:
  Surface() : value{nullptr, nullptr} {}
  void loadBMP(string path) {
    value = unique_ptr<SDL_Surface, decltype(&SDL_FreeSurface)>{
        sdl_ensure_not_null(SDL_LoadBMP(path.c_str())), SDL_FreeSurface};
  }

  SDL_Surface *get() { return value.get(); }

private:
  unique_ptr<SDL_Surface, decltype(&SDL_FreeSurface)> value;
};

class Texture {
public:
  Texture() : value{nullptr, nullptr} {}
  Texture(Renderer &f, Surface &s)
      : value{
            sdl_ensure_not_null(SDL_CreateTextureFromSurface(f.get(), s.get())),
            SDL_DestroyTexture} {}

  SDL_Texture *get() { return value.get(); }

private:
  unique_ptr<SDL_Texture, decltype(&SDL_DestroyTexture)> value;
};

int main() {
  SDL sdl(SDL_INIT_VIDEO);
  Window window{
      "Hello!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1920,
      1080,     SDL_WINDOW_SHOWN};
  Renderer ren{window, -1,
               SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC};

  std::string imagePath = "hello.bmp";
  Texture tex;
  {
    Surface bmp;
    bmp.loadBMP(imagePath);
    tex = Texture(ren, bmp);
  }

  SDL_Event e;
  bool quit = false;
  while (!quit) {
    SDL_RenderClear(ren.get());
    SDL_RenderCopy(ren.get(), tex.get(), NULL, NULL);
    SDL_RenderPresent(ren.get());
    while (SDL_PollEvent(&e)) {
      if (e.type == SDL_QUIT) {
        quit = true;
      }
      if (e.type == SDL_KEYDOWN) {
        quit = true;
      }
    }
  }

  return EXIT_SUCCESS;
}
