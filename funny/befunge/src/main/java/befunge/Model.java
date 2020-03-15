package befunge;
import java.util.Observable;
import java.util.Stack;
import java.util.Random;
import java.util.StringTokenizer;
public class Model extends Observable implements Runnable{
    public static final int width=80,height=25;
    private final char movers[]={'<','>','v','^'};
    private int x,y;
    private char data[][];
    private int dx,dy;
    private Stack<Integer>st;
    private Random gen;
    private boolean end;
    private boolean readMode;
    private String prog="";
    private int wait;
    public Model(){
        data=new char[width][height];
        clear();
        gen=new Random();
        st=new Stack<Integer>();
        reset();
        wait=0;
    }
    public void setProg(String newProg){
        prog=newProg;
    }
    private int pop(){
        if(st.empty())return 0;
        return st.pop();
    }
    private int top(){
        if(st.empty())return 0;
        return st.peek();
    }
    private void rotate(char mover){
        switch(mover){
            case '>':
                dx=1;
                dy=0;
                break;
            case '<':
                dx=-1;
                dy=0;
                break;
            case 'v':
                dx=0;
                dy=1;
                break;
            case '^':
                dx=0;
                dy=-1;
                break;
        }
    }
    private boolean good(int x,int y){
        return !(x<0||y<0||x>=width||y>=height);
    }
    public void move(){
        if(end)return;
        x+=dx;
        if(x<0)x+=width;
        else if(x>=width)x-=width;
        y+=dy;
        if(y<0)y+=height;
        else if(y>=height)y-=height;
        setChanged();
        notifyObservers();
    }

    public boolean execute(){
        boolean good=true;
        int a,b;
        int xPos,yPos;
        char val;
        int read;
        if(readMode){
            if(data[x][y]=='\"')readMode=false;
            else st.push((int) data[x][y]);
        }
        else if(data[x][y]>='0'&&data[x][y]<='9')st.push(data[x][y] - '0');
        else switch(data[x][y]){
            case '>':
            case '<':
            case 'v':
            case '^':
                rotate(data[x][y]);
                break;
            case '+':
                st.push(pop()+pop());
                break;
            case '-':
                a=pop();
                b=pop();
                st.push(b-a);
                break;
            case '*':
                st.push(pop()*pop());
                break;
            case '/':
                a=pop();
                b=pop();
                st.push(b/a);
                break;
            case '%':
                a=pop();
                b=pop();
                st.push(b%a);
                break;
            case '!':
                st.push(pop()==0?1:0);
                break;
            case '?':
                rotate(movers[gen.nextInt(4)]);
                break;
            case '_':
                if(pop()==0)rotate('>');
                else rotate('<');
                break;
            case '|':
                if(pop()==0)rotate('v');
                else rotate('^');
                break;
            case ':':
                st.push(top());
                break;
            case '$':
                pop();
                break;
            case '.':
                InOutPanel.getConsole().append(pop()+" ");
                break;
            case ',':
                InOutPanel.getConsole().append((char)pop()+"");
                break;
            case '#':
                move();
                break;
            case '@':
                end=true;
                break;
            case '`':
                st.push(pop()<pop()?1:0);
                break;
            case '\\':
                a=pop();
                b=pop();
                st.push(a);
                st.push(b);
                break;
            case 'p':
                yPos=pop();
                xPos=pop();
                if(!good(xPos,yPos))break;
                val=(char)pop();
                setChar(xPos,yPos,val);
                break;
            case 'g':
                yPos=pop();
                xPos=pop();
                if(!good(xPos,yPos)){
                    st.push((int)' ');
                    break;
                }
                st.push((int)getChar(xPos,yPos));
                break;
            case '\"':
                readMode=true;
                break;
            case '~':
                try{
                    InOutPanel.getConsole().setMode(2);
                    read=MyStream.getStream().read();
                    if(read==-1){
                        good=false;
                        break;
                    }
                    else{
                        InOutPanel.getConsole().setMode(0);
                        st.push(read);
                    }
                }catch(Exception ex){
                    ex.printStackTrace();
                }
                break;
            case '&':
                try{
                    InOutPanel.getConsole().setMode(1);
                    read=MyStream.getStream().read();
                    if(read==-1){
                        good=false;
                        break;
                    }
                    else{
                        InOutPanel.getConsole().setMode(0);
                        st.push(read);
                    }
                }catch(Exception ex){
                    ex.printStackTrace();
                }
                break;
            case ' ':
                break;
            default:
                System.err.println("Unexpected char: "+data[x][y]+". Will now halt.");
                //System.exit(1);
                break;
        }
        setChanged();
        notifyObservers();
        return good;
    }
    public void setChar(int posX,int posY,char ch){
        data[posX][posY]=ch;
        setChanged();
        notifyObservers();
    }
    public char getChar(int posX,int posY){
        return data[posX][posY];
    }
    public void reset(){
        x=0;
        y=0;
        dx=1;
        dy=0;
        st.clear();
        end=false;
        readMode=false;
        setChanged();
        notifyObservers();
    }
    public void clear(){
        for(int i=0;i<width;i++){
            for(int j=0;j<height;j++){
                data[i][j]=' ';
            }
        }
        setChanged();
        notifyObservers();
    }
    public int getX(){
        return x;
    }
    public int getY(){
        return y;
    }
    public void step(){
        if(execute())move();
    }
    public void setWait(int newWait){
        wait=newWait;
    }
    public void run(){
        while(!end){
            if(execute()){
                move();
                if(wait!=0)try{
                    Thread.sleep(wait);
                }catch(Exception e){
                    System.err.println("Something is very bad");
                }
            }
        }
    }
    public String getStack(){
        Object print[]=st.toArray();
        String ret="";
        for(int i=0;i<print.length;i++){
            ret+=(print[i]+" ");
        }
        if(ret.equals(""))ret="empty";
        return ret;
    }
    public void updateModel(){
        clear();
        StringTokenizer strtok=new StringTokenizer(prog,"\n");
        int count=0;
        String line;
        while(strtok.hasMoreTokens()){
            if(count>=Model.height)break;
            line=strtok.nextToken();
            for(int i=0;i<line.length()&&i<Model.width;i++){
                setChar(i,count,line.charAt(i));
            }
            count++;
        }
        reset();
    }
}
