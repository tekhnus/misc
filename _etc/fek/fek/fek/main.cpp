#include <SDL.h>
#include <SDL_ttf.h>
#include <cmath>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>
#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

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
  SDL() : nonzero{false} {}

  SDL(Uint32 flags) : nonzero{true} {
    if (SDL_Init(flags)) {
      throw runtime_error(SDL_GetError());
    }
    if (TTF_Init()) {
      throw runtime_error(TTF_GetError());
    }
  }

  SDL(SDL &&s) {
    nonzero = s.nonzero;
    s.nonzero = false;
  }

  SDL &operator=(SDL &&s) {
    nonzero = s.nonzero;
    s.nonzero = false;
    return *this;
  }

  ~SDL() {
    if (!nonzero) {
      return;
    }
    TTF_Quit();
    SDL_Quit();
  }

private:
  bool nonzero;
};

class Window {
public:
  Window() : value{nullptr, nullptr} {}
  Window(const string &t, int x, int y, int w, int h, Uint32 f)
      : value{sdl_ensure_not_null(SDL_CreateWindow(t.c_str(), x, y, w, h, f)),
              SDL_DestroyWindow} {}

  SDL_Window *get() { return value.get(); }

private:
  unique_ptr<SDL_Window, decltype(&SDL_DestroyWindow)> value;
};

class Renderer {
public:
  Renderer() : value{nullptr, nullptr} {}
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

class Saver;

class Buffer {
public:
  Buffer() : txt{} {}

  void bind_to_file(filesystem::path p);

  void insert(size_t pos, string text) { txt.insert(txt.begin() + pos, text); }

  void new_line(size_t pos) { insert(pos, "\n"); }

  void erase(size_t pos) {
    if (pos == 0) {
      return;
    }
    txt.erase(txt.begin() + pos - 1);
  }

  void set(const vector<string> &newtext) { txt = newtext; }

  const vector<string> &text() { return txt; }

private:
  vector<string> txt;
};

class ISyncer {
public:
  virtual void sync() = 0;
};

class TrivialSyncer : ISyncer {
  virtual void sync() override{};
};

class Syncer : ISyncer {
public:
  Syncer(Buffer &b, filesystem::path p) : buffer{b}, path{p} {}
  virtual void sync() override {
    fstream f{path, fstream::trunc};
    for (auto &s : buffer.text()) {
      f << s;
    }
  }

private:
  Buffer &buffer;
  filesystem::path path;
};

class Viewport {
public:
  Viewport(Buffer &buffer) : buffer{buffer}, cursor{} {}

  void render(Renderer &r, unordered_map<string, Texture> &table) {
    SDL_Rect dst{0, 0, 0, 0};
    Texture &x = table.at("x");
    SDL_QueryTexture(x.get(), NULL, NULL, &dst.w, &dst.h);
    symbolWidth = dst.w;
    SDL_SetRenderDrawColor(r.get(), 100, 255, 255, 255);
    int lineLength = 0;
    int lineOffset = 0;
    lineOffsets.clear();
    auto txt = buffer.text();
    for (auto pos = txt.begin();; ++pos) {
      if (pos == cursor + txt.begin()) {
        SDL_Rect currect{dst.x, dst.y, dst.w / 3, dst.h};
        SDL_RenderFillRect(r.get(), &currect);
      }
      ++lineLength;
      string cc;
      if (pos == txt.end() || (cc = *pos) == "\n") {
        lineOffsets.push_back({dst.y + dst.h / 2, lineOffset, lineLength});
        lineLength = 0;
        lineOffset = pos - txt.begin() + 1;
        lineLength = 0;
        dst.x = 0;
        dst.y += dst.h;
        if (pos == txt.end()) {
          break;
        }
        continue;
      }
      try {
        Texture &t = table.at(cc);
        SDL_RenderCopy(r.get(), t.get(), NULL, &dst);
        dst.x += dst.w;
      } catch (std::out_of_range &e) {
      }
    }
  }

  size_t get_offset_by_point(int x, int y) {
    auto it = lineOffsets.begin();
    for (; it != lineOffsets.end(); ++it) {
      if (get<0>(*it) >= y) {
        break;
      }
    }
    if (it == lineOffsets.end()) {
      if (it == lineOffsets.begin()) {
        throw runtime_error("lineOffsets cannot be empty");
      }
      --it;
    }
    if (it != lineOffsets.begin() &&
        y < (get<0>(*it) + get<0>(*(it - 1))) / 2) {
      --it;
    }
    int lineOffset = get<1>(*it);
    int lineLength = get<2>(*it);
    int column = round((double)x / symbolWidth);
    if (column >= lineLength) {
      column = lineLength - 1;
    }
    int pos = lineOffset + column;
    return pos;
  }

  void insert_before_cursor(const string &s) {
    buffer.insert(cursor, s);
    ++cursor;
  }

  void new_line_before_cursor() { insert_before_cursor("\n"); }

  void erase_before_cursor() {
    if (cursor != 0) {
      buffer.erase(cursor);
      --cursor;
    }
  }

  void move_cursor(int delta) {
    if ((long long)cursor + delta < 0) {
      cursor = 0;
      return;
    }
    cursor += delta;
    auto txt = buffer.text().size();
    if (cursor > txt) {
      cursor = txt;
    }
  }

  void set_cursor(size_t pos) { cursor = pos; }

private:
  Buffer &buffer;
  size_t cursor;
  vector<tuple<int, int, int>> lineOffsets;
  int symbolWidth;
};

SDL sdl;
Window window;
Renderer renderer;
unordered_map<string, Texture> font_cache;
Buffer buffer;
bool quit;
Viewport viewport{buffer};

void update() {
  SDL_SetRenderDrawColor(renderer.get(), 0, 0, 0, 0);
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
    if (e.type == SDL_QUIT) {
      quit = true;
    }
    if (e.type == SDL_TEXTINPUT) {
      viewport.insert_before_cursor(e.text.text);
    }
    if (e.type == SDL_KEYDOWN) {
      if (e.key.keysym.sym == SDLK_RETURN) {
        viewport.new_line_before_cursor();
      } else if (e.key.keysym.sym == SDLK_BACKSPACE) {
        viewport.erase_before_cursor();
      } else if (e.key.keysym.sym == SDLK_LEFT) {
        viewport.move_cursor(-1);
      } else if (e.key.keysym.sym == SDLK_RIGHT) {
        viewport.move_cursor(+1);
      }
    }
    if (e.type == SDL_MOUSEBUTTONDOWN) {
      size_t pos = viewport.get_offset_by_point(e.button.x, e.button.y);
      viewport.set_cursor(pos);
    }
  }
  SDL_RenderClear(renderer.get());
  viewport.render(renderer, font_cache);
  SDL_RenderPresent(renderer.get());
}

void update_w_logging() {
  try {
    update();
  } catch (exception &e) {
    cout << e.what() << endl;
    throw e;
  }
}

void font_cache_render() {
  Font f{"fonts/Menlo.ttc", 16};
  string rusalphabet[] = {"а", "б", "в", "г", "д", "е", "ж", "з", "и", "й", "к",
                          "л", "м", "н", "о", "п", "р", "с", "т", "у", "ф", "х",
                          "ц", "ч", "ш", "щ", "ъ", "ы", "ь", "э", "ю", "я"};
  for (string ch : rusalphabet) {
    Surface bmp;
    bmp.renderTextBlended(f, ch, {100, 255, 255, 255});
    font_cache[ch] = Texture{renderer, bmp};
  }
  for (char ch = 32; ch <= 126; ch++) {
    string chr{ch};
    Surface bmp;
    bmp.renderTextBlended(f, chr, {100, 255, 255, 255});
    font_cache[chr] = Texture{renderer, bmp};
  }
}

int main() {
  sdl = {SDL_INIT_VIDEO};
  window = {"Hello!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800,
            600,      SDL_WINDOW_SHOWN};
  renderer = {window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC};
  font_cache_render();
  buffer.set({"h", "e", "l", "l", "o", ",", " ", "w", "o", "r", "l", "d", "!"});

  SDL_StartTextInput();

#ifdef __EMSCRIPTEN__
  emscripten_set_main_loop(update_w_logging, 0, false);
#else
  while (!quit) {
    update_w_logging();
  }
#endif

  return EXIT_SUCCESS;
}
