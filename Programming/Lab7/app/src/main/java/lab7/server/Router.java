package lab7.server;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.logging.Level;
import java.util.logging.Logger;

import lab7.shared.messages.Request;
import lab7.shared.messages.Response;

/**
 * The Router class is responsible for routing commands to their corresponding
 * command handlers using multithreaded processing.
 */
public class Router {
    private final Worker worker;
    private final int PORT = 2224;
    private final int BUFFER_SIZE = 65535;
    private final DatagramChannel channel;
    private final ForkJoinPool forkJoinPool;
    private final ExecutorService processPool;
    private final ExecutorService sendPool;
    private static final Logger logger = Logger.getLogger(Router.class.getName());
// sockstat -4 -l | grep 2224
    /**
     * Default constructor for the Router class.
     * Initializes thread pools and network channel.
     */
    public Router() throws IOException {
        logger.info("[SERVER INIT] Initializing router components");
        logger.info("[NETWORK] Opening datagram channel");
        channel = DatagramChannel.open();
        logger.info(String.format("[NETWORK] Binding to port %d", PORT));
        channel.bind(new InetSocketAddress(PORT));
        channel.configureBlocking(false);
        worker = new Worker();
        forkJoinPool = ForkJoinPool.commonPool();
        processPool = Executors.newCachedThreadPool();
        sendPool = Executors.newFixedThreadPool(4);
        logger.info(String.format("[SERVER START] Ready and listening on port %d", PORT));
    }

    /**
     * Starts the server, reading requests using ForkJoinPool and blocking until
     * interrupted.
     */
    public void run() {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    readRequest();
                } catch (Exception e) {
                    logger.severe(String.format("[ERROR] Reading failed: %s", e.getMessage()));
                    logger.log(Level.WARNING, "Error details", e);
                }
            }
        }, forkJoinPool);

        try {
            // Block the main thread until the future completes or is interrupted
            future.join();
        } catch (Exception e) {
            logger.info("[SERVER] Server interrupted, shutting down");
            shutdown();
        }
    }

    /**
     * Reads a single request from the DatagramChannel and submits it for
     * processing.
     */
    private void readRequest() throws IOException, ClassNotFoundException {
        ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);
        buffer.clear();

        InetSocketAddress clientAddress = (InetSocketAddress) channel.receive(buffer);
        if (clientAddress == null) {
            return;
        }

        buffer.flip();
        logger.info("[PROCESSING] Deserializing incoming message");

        byte[] requestData = new byte[buffer.remaining()];
        buffer.get(requestData);

        ByteArrayInputStream byteInput = new ByteArrayInputStream(requestData);
        ObjectInputStream objectInput = new ObjectInputStream(byteInput);
        Request request = (Request) objectInput.readObject();

        logger.info(String.format("[PROCESSING] Received request: %s", request.toString()));

        // Submit request processing to CachedThreadPool
        processPool.submit(() -> processRequest(request, clientAddress));
    }

    /**
     * Processes a request and submits the response for sending.
     */
    private void processRequest(Request request, InetSocketAddress clientAddress) {
        try {
            Response response = worker.processCommand(request);
            logger.info(String.format("[PROCESSING] Processed request: %s", request.toString()));

            // Submit response sending to FixedThreadPool
            sendPool.submit(() -> sendResponse(response, clientAddress));
        } catch (Exception e) {
            logger.severe(String.format("[ERROR] Processing failed: %s", e.getMessage()));
            logger.log(Level.WARNING, "Error details", e);
        }
    }

    /**
     * Sends a response to the client.
     */
    private void sendResponse(Response response, InetSocketAddress clientAddress) {
        try {
            logger.info("[PROCESSING] Serializing response");
            ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
            ObjectOutputStream objectOutput = new ObjectOutputStream(byteOutput);
            objectOutput.writeObject(response);
            byte[] responseData = byteOutput.toByteArray();

            logger.info("[NETWORK] Sending response packet");
            ByteBuffer responseBuffer = ByteBuffer.wrap(responseData);
            channel.send(responseBuffer, clientAddress);
            logger.info(String.format("[NETWORK] Response sent to %s:%d",
                    clientAddress.getAddress().getHostAddress(),
                    clientAddress.getPort()));
        } catch (IOException e) {
            logger.severe(String.format("[ERROR] Sending response failed: %s", e.getMessage()));
            logger.log(Level.WARNING, "Error details", e);
        }
    }

    /**
     * Executes the command based on the provided request.
     *
     * @param request the request containing the command and its arguments
     * @return the response after executing the command
     */
    public Response runCommand(Request request) {
        return worker.processCommand(request);
    }

    /**
     * Shuts down the thread pools and closes the channel.
     */
    public void shutdown() {
        logger.info("[SERVER SHUTDOWN] Shutting down thread pools and channel");
        processPool.shutdown();
        sendPool.shutdown();
        forkJoinPool.shutdown();
        try {
            channel.close();
        } catch (IOException e) {
            logger.severe("[ERROR] Failed to close channel: " + e.getMessage());
        }
    }
}