package lab8.client.controllers.util;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import lab8.client.utils.Handler;
import lab8.shared.model.SpaceMarine;

import java.util.Stack;

public class DataSyncThread extends Thread {
    private final ObservableList<SpaceMarine> marineData;
    private final Runnable onDataUpdated;
    private volatile boolean running = true;
    private volatile boolean refreshNow = false;
    private Stack<SpaceMarine> lastQueue = new Stack<>();

    public DataSyncThread(ObservableList<SpaceMarine> marineData, Runnable onDataUpdated) {
        this.marineData = marineData;
        this.onDataUpdated = onDataUpdated;
        setDaemon(true);
    }

    @Override
    public void run() {
        while (running) {
            try {
                synchronized (Handler.getInstance()) {
                    Stack<SpaceMarine> marineQueue = Handler.getInstance().getCollection();
                    if (marineQueue != null && !marineQueue.isEmpty() && (!marineQueue.equals(lastQueue) || refreshNow)) {
                        lastQueue = new Stack<>();
                        lastQueue.addAll(marineQueue);
                        Platform.runLater(() -> {
                            marineData.setAll(marineQueue);
                            onDataUpdated.run();
                        });
                        refreshNow = false;
                    }
                }
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
    }

    public void refreshNow() {
        refreshNow = true;
    }

    public void shutdown() {
        running = false;
        interrupt();
    }
}