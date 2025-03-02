package lab5.io;

import java.util.Collection;

public interface Handler {
    public void writeCollection(Collection collection);

    public Collection readCollection();

}
