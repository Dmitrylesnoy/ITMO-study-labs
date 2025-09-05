package lab7.server;

import java.util.concurrent.*;

public class ThreadManager {
    public static final ForkJoinPool readPool = new ForkJoinPool();
    public static final ExecutorService processingPool = Executors.newCachedThreadPool();
    public static final ExecutorService responsePool = Executors.newFixedThreadPool(8); // например, 8 потоков
}
