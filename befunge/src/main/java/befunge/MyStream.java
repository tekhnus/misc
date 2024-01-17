package befunge;
import java.util.LinkedList;
public class MyStream{
    private LinkedList<Integer>q;
    private static MyStream inst=new MyStream();
    private MyStream(){
        super();
        q=new LinkedList<Integer>();
    }
    public synchronized int read(){
        if(q.size()==0)return -1;
        else return q.pollFirst();
    }
    public void push(int ch){
        q.push(ch);
    }
    public static MyStream getStream(){
        return inst;
    }
}
