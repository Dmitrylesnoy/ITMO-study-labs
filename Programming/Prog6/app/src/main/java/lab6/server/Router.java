package lab6.server;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import lab6.shared.messages.Request;
import lab6.shared.io.console.StdConsole;
import lab6.shared.messages.Response;

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
    private final int PORT = 2222;
    private final int BUFFER_SIZE = 65535;
    private DatagramChannel channel;
    private final int HEADER_SIZE = 12; // 4 bytes magic + 4 bytes msgId + 2 bytes total + 2 bytes index
    private final int MAX_PACKET_PAYLOAD = 1400; // conservative MTU-safe payload per UDP packet
    private final int CHUNK_DATA_SIZE = MAX_PACKET_PAYLOAD - HEADER_SIZE;
    private final Map<String, PartialMessage> partials = new ConcurrentHashMap<>();

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

            InetSocketAddress clientAddress = (InetSocketAddress) channel.receive(buffer);
            if (clientAddress == null) {
                return;
            }

            buffer.flip();
            int recLen = buffer.remaining();
            byte[] packetData = new byte[recLen];
            buffer.get(packetData);

            // Visible console log to help debug whether server receives packets
            try {
                StdConsole.writeln(
                        "[NETWORK] Packet received from " + clientAddress.toString() + " (" + recLen + " bytes)");
            } catch (Exception ignore) {
                System.out.println(
                        "[NETWORK] Packet received from " + clientAddress.toString() + " (" + recLen + " bytes)");
            }

            ByteBuffer headerBuf = ByteBuffer.wrap(packetData);
            int magic = headerBuf.getInt();
            int msgId = headerBuf.getInt();
            int totalParts = headerBuf.getShort() & 0xFFFF;
            int partIndex = headerBuf.getShort() & 0xFFFF;

            if (magic != 0x4D534731) { // 'MSG1'
                logger.warning("[NETWORK] Received packet with unknown magic, ignoring");
                return;
            }

            int dataOffset = HEADER_SIZE;
            int dataLen = packetData.length - dataOffset;
            byte[] chunk = new byte[dataLen];
            System.arraycopy(packetData, dataOffset, chunk, 0, dataLen);

            String key = clientAddress.toString() + "#" + msgId;
            PartialMessage pm = partials.computeIfAbsent(key, k -> new PartialMessage(totalParts));
            pm.addPart(partIndex, chunk);

            if (!pm.isComplete()) {
                // wait for remaining parts in subsequent runs
                return;
            }

            byte[] requestData = pm.assemble();
            partials.remove(key);

            logger.info("[PROCESSING] Deserializing incoming message");
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

            // split response into chunks and send back using same msgId
            int chunkDataSize = CHUNK_DATA_SIZE;
            int totalRespParts = (responseData.length + chunkDataSize - 1) / chunkDataSize;
            for (int i = 0; i < totalRespParts; i++) {
                int start = i * chunkDataSize;
                int end = Math.min(responseData.length, start + chunkDataSize);
                int len = end - start;
                ByteBuffer outBuf = ByteBuffer.allocate(HEADER_SIZE + len);
                outBuf.putInt(0x4D534731);
                outBuf.putInt(msgId);
                outBuf.putShort((short) totalRespParts);
                outBuf.putShort((short) i);
                outBuf.put(responseData, start, len);
                outBuf.flip();
                channel.send(outBuf, clientAddress);
                // try {
                //     StdConsole.writeln(String.format("[NETWORK] Sent response chunk %d/%d to %s:%d (%d bytes)",
                //             i + 1, totalRespParts, clientAddress.getAddress().getHostAddress(), clientAddress.getPort(),
                //             len));
                // } catch (Exception ignore) {
                //     logger.info(String.format("[NETWORK] Sent response chunk %d/%d to %s:%d (%d bytes)",
                //             i + 1, totalRespParts, clientAddress.getAddress().getHostAddress(), clientAddress.getPort(),
                //             len));
                // }
            }
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

    private static class PartialMessage {
        private final byte[][] parts;
        private final int total;
        private int received = 0;

        PartialMessage(int total) {
            this.total = Math.max(1, total);
            this.parts = new byte[this.total][];
        }

        synchronized void addPart(int idx, byte[] data) {
            if (idx < 0 || idx >= total)
                return;
            if (parts[idx] == null) {
                parts[idx] = data;
                received++;
            }
        }

        synchronized boolean isComplete() {
            return received >= total;
        }

        synchronized byte[] assemble() {
            int size = 0;
            for (byte[] p : parts)
                size += (p == null ? 0 : p.length);
            byte[] out = new byte[size];
            int pos = 0;
            for (int i = 0; i < total; i++) {
                byte[] p = parts[i];
                if (p == null)
                    continue;
                System.arraycopy(p, 0, out, pos, p.length);
                pos += p.length;
            }
            return out;
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
