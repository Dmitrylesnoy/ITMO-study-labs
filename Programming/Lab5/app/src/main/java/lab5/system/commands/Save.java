package lab5.system.commands;

import lab5.system.io.file.XMLhandler;
import lab5.system.utils.CollectionManager;

public class Save implements Command{
    private CollectionManager cm;

    public Save(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        cm.save();
    }

    @Override
    public String describe() {
        return "This command saves the collection to a file";
    }
    
}
