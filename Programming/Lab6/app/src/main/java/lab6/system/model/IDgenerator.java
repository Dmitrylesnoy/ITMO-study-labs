package lab6.system.model;

public class IDgenerator {
    private static long indexer = 0;

    public static long getNextId() {
        indexer = indexer + 1;
        return indexer;
    }

    public static void setIndexer(long id) {
        indexer = id;
    }
}
