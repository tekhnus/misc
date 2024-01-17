#include <cstdio>
#include <iostream>
#include <ncurses.h>
using namespace std;
class complex{
    private:
        double a,b;
    public:
        complex(double a,double b){
            this->a=a;
            this->b=b;
        }
        complex operator*(complex c){
            return complex(this->a*c.a-this->b*c.b,this->a*c.b+this->b*c.a);
        }
        complex operator+(complex c){
            return complex(this->a+c.a,this->b+c.b);
        }
        double abs(){
            return a*a+b*b;
        }
        double re(){
            return a;
        }
        double im(){
            return b;
        }
};
void drawMandel(double cX,double cY,double scale){
    int w=0;
    int h=0;
    getmaxyx(stdscr, w, h);
    w -= 10;
    h -= 3;
    bool pre[h][w];
    for(int r=-h/2;r<h/2;r++){
        for(int t=-w/2;t<w/2;t++){
            double x=cX+r/scale;
            double y=cY+t/scale;
            complex c(x,y);
            complex z(0,0);
            for(int i=0;i<100;i++)z=z*z+c;
            if(z.abs()<1e100)pre[r+h/2][t+w/2]=true;
            else pre[r+h/2][t+w/2]=false;
        }

    }
    for(int r=0;r<w;r++){
        for(int t=0;t<h;t++){
            printw("%c",pre[t][r]?'*':' ');
        }
        printw("\n");
    }
}
int main(void){
    double xc=-0.5,yc=0.0,scale=20.0;
    initscr();
    while(true){
        clear();
        drawMandel(xc,yc,scale);
        printw("-----------------\nwasd to move, q/e to scale:  \n");
        refresh();
        char inp;
        inp=getch();
        switch(inp){
            case 'a':
                xc-=3.0/scale;
                break;
            case 'd':
                xc+=3.0/scale;
                break;
            case 'w':
                yc-=3.0/scale;
                break;
            case 's':
                yc+=3.0/scale;
                break;
            case 'e':
                scale*=1.1;
                break;
            case 'q':
                scale/=1.1;
                break;
        }
    }
    endwin();
    return 0;
}
