#include <SDL.h>
#include <cstdlib>
#include <exception>
#include <iostream>

using namespace std;

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
      : value{SDL_CreateWindow(t.c_str(), x, y, w, h, f)} {
    if (!value) {
      throw runtime_error(SDL_GetError());
    }
  }

  ~Window() { SDL_DestroyWindow(value); }

  SDL_Window *get() { return value; }

private:
  SDL_Window *value;
};

class Renderer {
public:
  Renderer(SDL_Window *w, int x, Uint32 f)
      : value{SDL_CreateRenderer(w, x, f)} {
    if (!value) {
      throw runtime_error(SDL_GetError());
    }
  }
  ~Renderer() { SDL_DestroyRenderer(value); }

  SDL_Renderer *get() { return value; }

private:
  SDL_Renderer *value;
};

class Surface {
public:
  void loadBMP(string path) {
    value = SDL_LoadBMP(path.c_str());
    if (!value) {
      throw runtime_error(SDL_GetError());
    }
  }

  ~Surface() { SDL_FreeSurface(value); }

  SDL_Surface *get() { return value; }

private:
  SDL_Surface *value;
};

class Texture {
public:
  Texture() { value = nullptr; }
  Texture(SDL_Renderer *f, SDL_Surface *s)
      : value{SDL_CreateTextureFromSurface(f, s)} {
    if (!value) {
      throw runtime_error(SDL_GetError());
    }
  }

  Texture &operator=(Texture &&t) {
    value = t.value;
    t.value = nullptr;
    return *this;
  }

  ~Texture() { SDL_DestroyTexture(value); }

  SDL_Texture *get() { return value; }

private:
  SDL_Texture *value;
};

int main() {
  SDL sdl(SDL_INIT_VIDEO);
  Window window{
      "Hello!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1920,
      1080,     SDL_WINDOW_SHOWN};
  Renderer ren{window.get(), -1,
               SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC};

  std::string imagePath = "hello.bmp";
  Texture tex;
  {
    Surface bmp;
    bmp.loadBMP(imagePath);
    tex = Texture(ren.get(), bmp.get());
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
