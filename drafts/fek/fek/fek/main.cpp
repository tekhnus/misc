#include <SDL.h>
#include <SDL_ttf.h>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

template <typename T> T *sdl_ensure_not_null(T *ptr) {
  if (!ptr) {
    throw runtime_error(SDL_GetError());
  }
  return ptr;
}

template <typename T> T *ttf_ensure_not_null(T *ptr) {
  if (!ptr) {
    throw runtime_error(TTF_GetError());
  }
  return ptr;
}

class SDL {
public:
  SDL(Uint32 flags) {
    if (SDL_Init(flags)) {
      throw runtime_error(SDL_GetError());
    }
    if (TTF_Init()) {
      throw runtime_error(TTF_GetError());
    }
  }

  ~SDL() {
    TTF_Quit();
    SDL_Quit();
  }
};

class Window {
public:
  Window(const string &t, int x, int y, int w, int h, Uint32 f)
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

class Font {
public:
  Font(const string &n, int s)
      : value{sdl_ensure_not_null(TTF_OpenFont(n.c_str(), s)), TTF_CloseFont} {}

  TTF_Font *get() { return value.get(); }

private:
  unique_ptr<TTF_Font, decltype(&TTF_CloseFont)> value;
};

class Surface {
public:
  Surface() : value{nullptr, nullptr} {}
  void loadBMP(const string &path) {
    value = unique_ptr<SDL_Surface, decltype(&SDL_FreeSurface)>{
        sdl_ensure_not_null(SDL_LoadBMP(path.c_str())), SDL_FreeSurface};
  }
  void renderTextBlended(Font &f, const string &s, SDL_Color c) {
    value = unique_ptr<SDL_Surface, decltype(&SDL_FreeSurface)>{
        ttf_ensure_not_null(TTF_RenderUTF8_Blended(f.get(), s.c_str(), c)),
        SDL_FreeSurface};
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

void drawText(Renderer &r, const vector<string> &text,
              unordered_map<string, Texture> &table) {
  SDL_Rect dst{0, 0, 0, 0};
  Texture &x = table.at("x");
  SDL_QueryTexture(x.get(), NULL, NULL, &dst.w, &dst.h);
  SDL_SetRenderDrawColor(r.get(), 100, 255, 255, 255);
  for (string cc : text) {
    if (cc == "\n") {
      dst.x = 0;
      dst.y += dst.h;
      continue;
    }
    try {
      Texture &t = table.at(cc);
      SDL_RenderCopy(r.get(), t.get(), NULL, &dst);
      dst.x += dst.w;
    } catch (std::out_of_range &e) {
    }
  }
  SDL_RenderFillRect(r.get(), &dst);
}

int main() {
  SDL sdl(SDL_INIT_VIDEO);
  Window window{"Hello!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800,
                600,      SDL_WINDOW_SHOWN};
  Renderer ren{window, -1,
               SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC};

  unordered_map<string, Texture> table;
  Texture tex;
  {
    string rusalphabet[] = {"а", "б", "в", "г", "д", "е", "ж", "з",
                            "и", "й", "к", "л", "м", "н", "о", "п",
                            "р", "с", "т", "у", "ф", "х", "ц", "ч",
                            "ш", "щ", "ъ", "ы", "ь", "э", "ю", "я"};
    Font f{"/System/Library/Fonts/Menlo.ttc", 14};
    for (string ch : rusalphabet) {
      Surface bmp;
      bmp.renderTextBlended(f, ch, {100, 255, 255, 255});
      table[ch] = Texture{ren, bmp};
    }
    for (char ch = 32; ch <= 126; ch++) {
      string chr{ch};
      Surface bmp;
      bmp.renderTextBlended(f, chr, {100, 255, 255, 255});
      table[chr] = Texture{ren, bmp};
    }
  }

  SDL_Event e;
  bool quit = false;
  SDL_StartTextInput();
  vector<string> text = {"h", "e", "l", "l", "o", ",", " ",
                         "w", "o", "r", "l", "d", "!"};
  while (!quit) {
    SDL_SetRenderDrawColor(ren.get(), 0, 0, 0, 0);
    SDL_RenderClear(ren.get());
    drawText(ren, text, table);
    SDL_Rect dst{};
    SDL_QueryTexture(tex.get(), NULL, NULL, &dst.w, &dst.h);
    SDL_RenderCopy(ren.get(), tex.get(), NULL, &dst);
    SDL_RenderPresent(ren.get());
    while (SDL_PollEvent(&e)) {
      if (e.type == SDL_QUIT) {
        quit = true;
      }
      if (e.type == SDL_TEXTINPUT) {
        string s = e.text.text;
        text.push_back(s);
      }
      if (e.type == SDL_KEYDOWN) {
        if (e.key.keysym.sym == SDLK_RETURN) {
          text.push_back("\n");
        } else if (e.key.keysym.sym == SDLK_BACKSPACE) {
          text.pop_back();
        }
      }
    }
  }

  return EXIT_SUCCESS;
}
