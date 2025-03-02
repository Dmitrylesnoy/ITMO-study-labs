package lab5.commands;

import java.util.logging.Handler;

import lab5.io.*;
import lab5.utils.CollectionManager;

public class Load implements Command {
    private CollectionManager cm;

    public Load(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        XMLhandler handler = new XMLhandler();
        // cm.setCollection(handler.readCollection());    
    }

    @Override
    public String describe() {
        return "This command loads the collection from a file";
    }

}
