package lab5.commands;

import lab5.utils.CollectionManager;
import lab5.utils.XMLhandler;

public class Save implements Command{
    private CollectionManager cm;

    public Save(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        XMLhandler handler = new XMLhandler();
        handler.writeStackToXML(cm.getCollection());
    }

    @Override
    public String describe() {
        return "This command saves the collection to a file";
    }
    
}
