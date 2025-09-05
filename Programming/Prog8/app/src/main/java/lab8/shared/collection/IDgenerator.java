package lab8.shared.collection;

import java.util.Collection;
import java.util.stream.Collectors;

import lab8.shared.model.SpaceMarine;

public class IDgenerator {
    private long indexer = 0;

    public long getNextId() {
        indexer = indexer + 1;
        return indexer;
    }

    public void setIndexer(long id) {
        indexer = id;
    }

    public void updateIndexer(Collection<SpaceMarine> collection) {
        if (collection.size() == 0)
            indexer = 1;
        else {
            indexer = collection.stream().filter(m -> m.getId() != null).mapToLong(m -> m.getId()).max().orElse(0);
            collection = collection.stream().filter(m -> m.getId() == null).peek(m -> m.setId(getNextId()))
                    .collect(Collectors.toList());
        }
    }
}
