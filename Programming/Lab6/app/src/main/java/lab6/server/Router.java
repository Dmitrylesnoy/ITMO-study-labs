package lab6.server;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.ArrayDeque;
import java.util.Deque;

import lab6.system.collection.CollectionManager;
import lab6.system.commands.Command;
import lab6.system.io.console.StdConsole;
import lab6.system.messages.Request;
import lab6.system.messages.Response;

/**
 * The Router class is responsible for routing commands to their corresponding
 * command handlers.
 * It maintains a collection of commands and executes them based on user
 * requests.
 * This class implements the Singleton pattern to ensure that only one instance
 * of Router exists.
 */
public class Router {
    private CollectionManager cm;
    private Deque<Request> cmdsQueue;
    private Worker worker1;
    private final int PORT = 2222;
    private final int BUFFER_SIZE = 65535;
    private DatagramChannel channel;

    /**
     * Default constructor for the Router class.
     * Initializes the Router instance and loads the collection manager.
     */
    public Router() throws IOException{
        cm = CollectionManager.getInstance();
        cm.load();
        cmdsQueue = new ArrayDeque<Request>(1);
        channel = DatagramChannel.open();
        channel.bind(new InetSocketAddress((PORT)));
        worker1 = new Worker();
        StdConsole.writeln("server started on port"+PORT);
    }

    public void run() {
        try {
            ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);
            buffer.clear();

            StdConsole.writeln("    Waiting packet");
            InetSocketAddress clientAddress = (InetSocketAddress) channel.receive(buffer);
            buffer.flip();

            StdConsole.writeln("    Deseralization message");
            byte[] requestData=new byte[buffer.remaining()];
            buffer.get(requestData);
            ByteArrayInputStream byteInput = new ByteArrayInputStream(requestData);
            ObjectInputStream objectInput = new ObjectInputStream(byteInput);
            Request request = (Request) objectInput.readObject();

            StdConsole.writeln("    processing request");
            cmdsQueue.add(request);
            Response response = worker1.processCommand(cmdsQueue.pop());

            StdConsole.writeln("    Serialize responce ");
            ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
            ObjectOutputStream objectOutput = new ObjectOutputStream(byteOutput);
            objectOutput.writeObject(response);
            byte[] responceData = byteOutput.toByteArray();

            StdConsole.writeln("    Sending answer packet");
            ByteBuffer responceBuffer = ByteBuffer.wrap(responceData);
            channel.send(responceBuffer, clientAddress);
            StdConsole.writeln("Response sent to " + clientAddress.getAddress() + ":" + clientAddress.getPort());        } catch (NullPointerException e){
            StdConsole.writeln("Client disconnected");
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
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
        cmdsQueue.add(request);
        Response response = worker1.processCommand(cmdsQueue.pop());
        return response;
    }
}
