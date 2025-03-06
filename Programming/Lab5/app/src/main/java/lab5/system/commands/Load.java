package lab5.system.commands;

import java.util.logging.Handler;

import lab5.system.io.*;
import lab5.system.io.file.XMLhandler;
import lab5.system.utils.CollectionManager;

public class Load implements Command {
    private CollectionManager cm;

    public Load(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        cm.load();
    }

    @Override
    public String describe() {
        return "This command loads the collection from a file";
    }

}
