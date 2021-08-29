#include <SDL.h>
#include <cstdlib>
#include <iostream>

using namespace std;

int main() {
  if (SDL_Init(SDL_INIT_VIDEO)) {
    cerr << SDL_GetError() << endl;
    return EXIT_FAILURE;
  }
  SDL_Window* window = SDL_CreateWindow("Hello!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1920, 1080, SDL_WINDOW_SHOWN);
  if (!window) {
    cerr << SDL_GetError() << endl;
    SDL_Quit();
    return EXIT_FAILURE;
  }
  SDL_Renderer* ren = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (!ren) {
    cerr << SDL_GetError() << endl;
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_FAILURE;
  }
  std::string imagePath = "hello.bmp";
  SDL_Surface *bmp = SDL_LoadBMP(imagePath.c_str());
  if (bmp == nullptr){
    cerr << SDL_GetError() << endl;
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_FAILURE;
  }
  SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == nullptr){
    cerr << SDL_GetError() << endl;
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_FAILURE;
  }
  SDL_Event e;
  bool quit = false;
  while (!quit){
    SDL_RenderClear(ren);
    SDL_RenderCopy(ren, tex, NULL, NULL);
    SDL_RenderPresent(ren);
    while (SDL_PollEvent(&e)){
      if (e.type == SDL_QUIT){
	quit = true;
      }
      if (e.type == SDL_KEYDOWN){
	quit = true;
      }
    }
  }
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(ren);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return EXIT_SUCCESS;
}
