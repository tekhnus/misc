package game;
import javafx.stage.Stage;
import javafx.scene.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.*;
import javafx.util.Math;
import javafx.scene.text.*;
import javafx.scene.effect.*;
import javafx.scene.effect.light.*;
var n=10;
var sz=60;
var field=[0..n*n-1];
for(i in [0..<n*n])field[i]=0;
field[0]=1;
field[n*n-1]=1;
field[n-1]=2;
field[n*n-n]=2;
var player=1;
var chosen=-1;
var endgame=-1;
function calc(){
    var pts=[0,0];
    var full=true;
    for(i in [0..<n*n]){
        if(field[i]==0){full=false;}
        else pts[field[i]-1]+=1;
    }
    if(full)return if(pts[0]>pts[1]){1} else {2};
    for(pl in [1..2])if(pts[pl-1]==0)return (3-pl);
    return -1;
}
function turn(place:Integer){
    field[place]=player;
    chosen=-1;
    for(i in [place/n-1..place/n+1]){
        for(c in [(place mod n)-1..(place mod n)+1]){
            if(i<0 or i>=n or c<0 or c>=n or (i==place/n and c==(place mod n))){
                continue;
            }
            if(field[i*n+c]==(3-player)){
                field[i*n+c]=player;
            }
        }
    }
    var res=calc();
    if(res!=-1){
        endgame=res;
        print(res);
    }
    player=3-player;
}
var cont=bind[
            for(i in [0..<n]){
                for(c in [0..<n]){
                    Group{
                        content:bind[
                            Rectangle{
                                fill:if(i*n+c==chosen)Color.GREY else Color.DARKGREY
                                width:sz-2
                                height:sz-2
                                x:i*sz
                                y:c*sz
                            },
                            if(field[i*n+c]!=0)Circle{
                                centerX:i*sz+sz/2-1
                                centerY:c*sz+sz/2-1
                                radius:4*sz/9
                                fill:if(field[i*n+c]==1){Color.ORANGERED} else {Color.BLUEVIOLET}
                                effect:DropShadow{input:InnerShadow{radius:5 input:Lighting{light:DistantLight{elevation:60}}}}
                            }
                            else null
                        ]
                        onMouseClicked:function(ev){
                            if(chosen==-1 and field[i*n+c]==player)chosen=i*n+c
                            else if(chosen==-1){}
                            else if(chosen==i*n+c)chosen=-1
                            else if(Math.abs(chosen/n-i)<=1 and Math.abs((chosen mod n)-c)<=1 and field[i*n+c]==0){
                                turn(i*n+c);
                            }
                            else if(Math.abs(chosen/n-i)<=2 and Math.abs((chosen mod n)-c)<=2 and field[i*n+c]==0){
                                field[chosen]=0;
                                turn(i*n+c);
                            }
                            else {}
                        }
                    }
                }
            },
            if(endgame!=-1)Text{content:"Player {endgame} wins!" x:sz*n/3 y:sz*n/2 font:Font{size:30}} else null
        ];
Stage{
    title:"Game"
    scene:Scene{
        width:sz*n
        height:sz*n
        content:bind cont
        fill:Color.GREY
    }
}