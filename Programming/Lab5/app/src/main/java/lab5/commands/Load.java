package lab5.commands;

import lab5.utils.CollectionManager;
import lab5.utils.XMLhandler;

public class Load implements Command {
    private CollectionManager cm;

    public Load(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        XMLhandler handler = new XMLhandler();
        cm.setCollection(handler.readStackFromXML());    }

    @Override
    public String describe() {
        return "This command loads the collection from a file";
    }

}
