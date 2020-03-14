package befunge;
public class Main{
    public static void main(String[] args){
        Model model=new Model();
        RunFrame run=new RunFrame(model);
        EditFrame edit=new EditFrame(model);
        run.addFriend(edit);
        edit.addFriend(run);
    }
}
