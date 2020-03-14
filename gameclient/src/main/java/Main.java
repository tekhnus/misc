import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
class cVars{
    public static final int w=600;
    public static final int sz=15;
    public static final String ip="91.76.27.20";
    public static int port=6633;
    public static final int n=2;
}
class Field extends JPanel implements MouseListener,MouseMotionListener{
    final int w=cVars.w;
    final int sz=cVars.sz;
    final int n=cVars.n;
    int field[][]=new int[sz][sz];
    int ourNum;
    //char symbols[]={' ','x','o','q'};
    Color colors[]={Color.LIGHT_GRAY,new Color(153,0,51),new Color(0,153,102),new Color(0,128,153)};
    Socket sock;
    BufferedWriter str_out;
    BufferedReader str_in;
    boolean need;
    Worker worker;
    int counter;
    int ax=0;
    int ay=0;
    Chat chat;
    void init(){
        counter=1;
        need=false;
        for(int i=0;i<cVars.sz;i++){
            for(int j=0;j<cVars.sz;j++){
                field[i][j]=0;
            }
        }
    }
    Field(){
        //setSize(w,w);
        setLayout(null);
        sock=null;
        //
        //
        try{
            sock=new Socket();
            sock.connect(new InetSocketAddress(cVars.ip,cVars.port));
            System.out.println("First connect OK");
            str_out=new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));
            str_in=new BufferedReader(new InputStreamReader(sock.getInputStream()));
            cVars.port=Integer.parseInt(str_in.readLine());
            //str_in.close();
            //str_out.close();
        }
        catch(Exception ex){
            System.err.println("First connect fail.");
        }
        System.out.println(cVars.port);
        //
        //
        try{
            sock=new Socket();
            sock.connect(new InetSocketAddress(cVars.ip,cVars.port));
            str_out=new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));
            str_in=new BufferedReader(new InputStreamReader(sock.getInputStream()));
        }
        catch(Exception ex){
            System.err.println("Second connect fail.");
        }
        init();
        addMouseListener(this);
        addMouseMotionListener(this);
        chat=new Chat();
        chat.setBounds(5,w+30,w,w/3);
        add(chat);
        worker=new Worker(this);
        worker.start();
    }
    public void msg(String s,String head){
        JOptionPane.showMessageDialog(this,s,head,JOptionPane.INFORMATION_MESSAGE);
    }

    public void mouseDragged(MouseEvent e){}
    public void mouseMoved(MouseEvent e){
        int x=e.getX();
        int y=e.getY();
        int tx=x*sz/w;
        int ty=y*sz/w;
        if(tx>=sz||ty>=cVars.sz)return;
        ax=tx;
        ay=ty;
        repaint();
    }
    class Worker extends Thread{
        Field master;
        Worker(Field mst){
            master=mst;
        }
        @Override
        public void run(){
            while(true){
                try{
                    String get=str_in.readLine();
                    if(get!=null&&!get.equals("\n")&&!get.equals("")){
                        if(get.charAt(0)=='s'){
                            ourNum=get.charAt(1)-'0';
                            System.out.println(ourNum);
                        }
                        else if(get.charAt(0) == 't'){
                            System.out.println("Ours!");
                            need=true;
                        }
                        else if(get.charAt(0)=='x'){
                            StringTokenizer st=new StringTokenizer(get,"-");
                            st.nextToken();
                            int who=Integer.parseInt(st.nextToken());
                            int x=Integer.parseInt(st.nextToken());
                            int y=Integer.parseInt(st.nextToken());
                            field[y][x]=who;
                            counter=(counter+1);
                            if(counter>n)counter=1;
                            repaint();
                        }
                        else if(get.charAt(0)=='w'){
                            chat.chat.append("Congratulations to player "+(get.charAt(1)-'0')+'\n');
                            if(get.charAt(1)-'0'==ourNum){
                                msg("Congratulations!Click OK to restart","Victory");
                                str_out.write("r\n");
                                str_out.flush();
                            }
                        }
                        else if(get.charAt(0)=='n'){
                            chat.chat.append("Draw!Worderful, brilliant, fantastic game!\n");
                        }
                        else if(get.charAt(0)=='c'){
                            chat.chat.append(get.substring(1)+'\n');
                        }
                        if(get.charAt(0)=='r'){
                            init();
                            repaint();
                            chat.chat.append("---New game started---\n");
                        }
                    }
                }catch(Exception ex){
                    System.err.println("Exception while processing a message");
                }
            }
        }
    }
    class Chat extends JPanel implements ActionListener,KeyListener{
        JTextArea chat;
        JTextField inp;
        JButton ok;
        Chat(){
            setLayout(new GridLayout(3,1,5,5));
            chat=new JTextArea("",300,5);
            chat.setEditable(false);
            JScrollPane sp=new JScrollPane(chat);
            sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            add(sp);
            inp=new JTextField(40);
            ok=new JButton("Send");
            ok.addActionListener(this);
            inp.addKeyListener(this);
            JPanel tmp=new JPanel();
            tmp.add(new JLabel("Message:"));
            tmp.add(inp);
            tmp.add(ok);
            add(tmp);
        }
        public void actionPerformed(ActionEvent e){
            msg();
        }
        public void msg(){
            try{
                str_out.write("c"+inp.getText()+"\n");
                str_out.flush();
                inp.setText("");
            }catch(Exception ex){
                System.err.println("Exception when sending chat message");
            }
        }
        public void keyTyped(KeyEvent e){}
        public void keyPressed(KeyEvent e){
            if(e.getKeyCode()==KeyEvent.VK_ENTER)msg();
        }
        public void keyReleased(KeyEvent e){}
    }
    @Override
    public void paint(Graphics gr){
        super.paint(gr);
        Graphics2D g=(Graphics2D)gr;
        g.setStroke(new BasicStroke(3));
        g.setFont(new Font("Arial",Font.PLAIN,cVars.w/cVars.sz));
        g.setColor(Color.LIGHT_GRAY);
        g.fillRect(0,0,w,w);
        //g.setColor(Color.WHITE);
        //g.fillRect(w,0,3*w/sz,3*w/sz);
        //
        g.setColor(Color.GRAY);
        g.fillRect(ax*w/sz,ay*w/sz,w/sz,w/sz);
        //
        g.setColor(Color.DARK_GRAY);
        for(int i=0;i<=sz;i++){
            g.drawLine(i*w/sz,0,i*w/sz,w);
            g.drawLine(0,i*w/sz,w,i*w/sz);
        }
        g.setStroke(new BasicStroke(6));
        for(int y=0;y<sz;y++){
            for(int x=0;x<sz;x++){
                g.setColor(colors[field[y][x]]);
                drawFig(field[y][x],x,y,g);
            }
        }
        drawCount(g);
    }
    public void drawFig(int fn,int x,int y,Graphics2D g){
        double scale=0.7;
        int sdv=(int)(w/sz*(1-scale)/2);
        if(fn==1){
            g.drawLine(x*w/sz+sdv,y*w/sz+sdv,(x+1)*w/sz-sdv,(y+1)*w/sz-sdv);
            g.drawLine((x+1)*w/sz-sdv,y*w/sz+sdv,x*w/sz+sdv,(y+1)*w/sz-sdv);
        }
        else if(fn==2){
            g.drawOval(x*w/sz+sdv,y*w/sz+sdv,(int)(w/sz*scale),(int)(w/sz*scale));
        }
        else if(fn==3){
            g.drawLine(x*w/sz+sdv,(y+1)*w/sz-sdv,x*w/sz+w/sz/2,y*w/sz+sdv);
            g.drawLine(x*w/sz+w/sz/2,y*w/sz+sdv,(x+1)*w/sz-sdv,(y+1)*w/sz-sdv);
            g.drawLine((x+1)*w/sz-sdv,(y+1)*w/sz-sdv,x*w/sz+sdv,(y+1)*w/sz-sdv);
        }
    }
    public void drawCount(Graphics2D g){
        g.setColor(colors[counter]);
        drawFig(counter,sz+1,2,g);
        g.setColor(Color.BLACK);
        g.drawString("turn",w+2*w/sz/3,3*w/sz/2);
    }
    public void mouseClicked(MouseEvent e){
        if(need){
            int x=e.getX()*sz/w;
            int y=e.getY()*sz/w;
            //if(x>=cVars.sz||y>=cVars.sz)return;
            if(field[y][x]==0){
                try{
                    System.out.println(x+" "+y);
                    str_out.write(x+"-"+y+"\n");
                    str_out.flush();
                    need=false;
                }catch(Exception ex){
                    System.err.println("Exception when sending a move");
                }
            }
        }
    }
    public void mousePressed(MouseEvent e){}
    public void mouseReleased(MouseEvent e){}
    public void mouseEntered(MouseEvent e){}
    public void mouseExited(MouseEvent e){}
}
class MainFrame extends JFrame{    
    Field comp;
    MainFrame(){
        super("XO by harius");
        setSize(cVars.w+3*cVars.w/cVars.sz+10,cVars.w+cVars.w/4+50);
        this.setResizable(false);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //setLayout(null);
        comp=new Field();
        comp.setBounds(10,10,cVars.w,cVars.w);
        add(comp);
        setVisible(true);
    }
}
public class Main {
    public static void main(String[] args){
        new MainFrame();
    }
}
