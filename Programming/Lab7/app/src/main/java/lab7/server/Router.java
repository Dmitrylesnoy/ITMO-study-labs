package lab7.server;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.logging.Level;
import java.util.logging.Logger;

import lab7.shared.messages.Request;
import lab7.shared.messages.Response;

/**
 * The Router class is responsible for routing commands to their corresponding
 * command handlers.
 * It maintains a collection of commands and executes them based on user
 * requests.
 * This class implements the Singleton pattern to ensure that only one instance
 * of Router exists.
 */
public class Router {
    // private Deque<Request> cmdsQueue;
    private Worker worker1;
    private final int PORT = 2224;
    private final int BUFFER_SIZE = 65535;
    private DatagramChannel channel;

    /**
     * Default constructor for the Router class.
     * Initializes the Router instance and loads the collection manager.
     */
    private static final Logger logger = Logger.getLogger(Router.class.getName());

    public Router() throws IOException {
        logger.info("[SERVER INIT] Initializing router components");
        // cmdsQueue = new ArrayDeque<Request>(1);
        logger.info("[NETWORK] Opening datagram channel");
        channel = DatagramChannel.open();
        logger.info(String.format("[NETWORK] Binding to port %d", PORT));
        channel.bind(new InetSocketAddress(PORT));
        channel.configureBlocking(false);
        worker1 = new Worker();
        logger.info(String.format("[SERVER START] Ready and listening on port %d", PORT));
    }

    public void run() {
        ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);

        try {
            buffer.clear();
            // StdConsole.writeln(" Waiting packet");

            InetSocketAddress clientAddress = (InetSocketAddress) channel.receive(buffer);
            if (clientAddress == null) {
                // StdConsole.writeln("Received null client address, skipping...");
                return;
            }

            buffer.flip();
            logger.info("[PROCESSING] Deserializing incoming message");

            byte[] requestData = new byte[buffer.remaining()];
            buffer.get(requestData);

            Request request;
            ByteArrayInputStream byteInput = new ByteArrayInputStream(requestData);
            ObjectInputStream objectInput = new ObjectInputStream(byteInput);
            request = (Request) objectInput.readObject();
            
            
            logger.info(String.format("[PROCESSING] Executing request: %s", request.toString()));
            Response response = worker1.processCommand(request);

            logger.info("[PROCESSING] Serializing response");
            byte[] responseData;
            ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
            ObjectOutputStream objectOutput = new ObjectOutputStream(byteOutput);
            objectOutput.writeObject(response);
            responseData = byteOutput.toByteArray();

            logger.info("[NETWORK] Sending response packet");
            ByteBuffer responseBuffer = ByteBuffer.wrap(responseData);
            channel.send(responseBuffer, clientAddress);
            logger.info(String.format("[NETWORK] Response sent to %s:%d",
                    clientAddress.getAddress().getHostAddress(),
                    clientAddress.getPort()));
        } catch (NullPointerException e) {
            logger.warning("[CLIENT] Client connection terminated unexpectedly");
            logger.log(Level.WARNING, "Client disconnect details", e);
        } catch (Exception e) {
            logger.severe(String.format("[ERROR] Processing failed: %s", e.getMessage()));
            logger.log(Level.WARNING, "Error details", e);
        }
    }

    /**
     * Executes the command based on the provided request.
     *
     * @param request the request containing the command and its arguments
     * @return the response after executing the command
     * @throws IOException
     * @throws FileNotFoundException
     * @throws ClassNotFoundException
     */
    public Response runCommand(Request request) {
        // cmdsQueue.add(request);
        Response response = worker1.processCommand(request);
        return response;
    }
}
