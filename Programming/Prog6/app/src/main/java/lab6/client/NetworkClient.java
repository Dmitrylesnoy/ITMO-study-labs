package lab6.client;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.util.Random;

import lab6.shared.io.console.StdConsole;
import lab6.shared.messages.Request;
import lab6.shared.messages.Response;
import lab6.shared.messages.Status;

public class NetworkClient {
    private static final String SERVER_IP = "192.168.10.80";// "77.234.196.4"; //"127.0.0.1"; // Пример //
                                                            // "192.168.10.80"
                                                            // IP сервера
    private static final int SERVER_PORT = 2222; // Пример порта сервера
    private static final int TIMEOUT = 5000; // Тайм-аут в миллисекундах
    private static final int MAX_ATTEMPTS = 3; // Максимальное количество попыток

    final int HEADER_SIZE = 12; // 4 magic + 4 msgId + 2 total + 2 index
    final int MAX_PACKET_PAYLOAD = 1400;
    final int CHUNK_DATA_SIZE = MAX_PACKET_PAYLOAD - HEADER_SIZE;

    public Response sendRequest(Request request) {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(TIMEOUT); // Устанавливаем тайм-аут

            // Сериализация Request в массив байтов
            ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
            ObjectOutputStream objectOutput = new ObjectOutputStream(byteOutput);
            objectOutput.writeObject(request);
            byte[] requestData = byteOutput.toByteArray();
            InetAddress serverAddress = InetAddress.getByName(SERVER_IP);

            int msgId = new Random().nextInt();
            int totalParts = (requestData.length + CHUNK_DATA_SIZE - 1) / CHUNK_DATA_SIZE;

            Response response = null;
            for (int attempt = 1; attempt <= MAX_ATTEMPTS; attempt++) {
                // send all parts for this attempt
                for (int i = 0; i < totalParts; i++) {
                    int start = i * CHUNK_DATA_SIZE;
                    int end = Math.min(requestData.length, start + CHUNK_DATA_SIZE);
                    int len = end - start;
                    ByteBuffer outBuf = ByteBuffer.allocate(HEADER_SIZE + len);
                    outBuf.putInt(0x4D534731); // 'MSG1'
                    outBuf.putInt(msgId);
                    outBuf.putShort((short) totalParts);
                    outBuf.putShort((short) i);
                    outBuf.put(requestData, start, len);
                    DatagramPacket packet = new DatagramPacket(outBuf.array(), 0, outBuf.position(), serverAddress,
                            SERVER_PORT);
                    socket.send(packet);
                    // StdConsole.writeln("Sent chunk " + (i + 1) + "/" + totalParts + " (" + outBuf.position() + " bytes)");
                    try {
                        Thread.sleep(10);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                    }
                }

                // wait for response for this attempt
                long deadline = System.currentTimeMillis() + TIMEOUT; // base wait for this attempt
                byte[][] respParts = null;
                int respTotal = -1;

                while (System.currentTimeMillis() < deadline) {
                    try {
                        byte[] buffer = new byte[65535];
                        DatagramPacket responsePacket = new DatagramPacket(buffer, buffer.length);
                        socket.receive(responsePacket);

                        ByteBuffer inBuf = ByteBuffer.wrap(responsePacket.getData(), 0, responsePacket.getLength());
                        int magic = inBuf.getInt();
                        int rcvMsgId = inBuf.getInt();
                        int rTotal = inBuf.getShort() & 0xFFFF;
                        int rIndex = inBuf.getShort() & 0xFFFF;

                        if (magic != 0x4D534731)
                            continue;
                        if (rcvMsgId != msgId)
                            continue; // ignore other messages

                        int dataLen = responsePacket.getLength() - HEADER_SIZE;
                        byte[] chunk = new byte[dataLen];
                        inBuf.get(chunk);
                        // StdConsole.writeln("Received chunk " + rIndex + "/" + rTotal + " (" + dataLen + " bytes)");

                        // reset sliding deadline on every received chunk so slow transfers can complete
                        deadline = System.currentTimeMillis() + TIMEOUT;

                        if (respParts == null) {
                            respTotal = rTotal;
                            respParts = new byte[respTotal][];
                            // extend overall deadline based on expected number of parts
                            long extra = Math.max(1, respTotal);
                            deadline = System.currentTimeMillis() + TIMEOUT * extra;
                            // StdConsole.writeln("Extended response wait deadline to allow " + respTotal
                            //         + " parts (deadline in " + (TIMEOUT * extra) + " ms)");
                        }
                        if (rIndex >= 0 && rIndex < respTotal && respParts[rIndex] == null) {
                            respParts[rIndex] = chunk;
                        }

                        boolean complete = true;
                        if (respParts != null) {
                            for (int i = 0; i < respTotal; i++) {
                                if (respParts[i] == null) {
                                    complete = false;
                                    break;
                                }
                            }
                        } else
                            complete = false;

                        if (complete) {
                            // assemble
                            int totalSize = 0;
                            for (byte[] p : respParts)
                                totalSize += p.length;
                            byte[] respData = new byte[totalSize];
                            int pos = 0;
                            for (int i = 0; i < respTotal; i++) {
                                System.arraycopy(respParts[i], 0, respData, pos, respParts[i].length);
                                pos += respParts[i].length;
                            }

                            ByteArrayInputStream byteInput = new ByteArrayInputStream(respData);
                            ObjectInputStream objectInput = new ObjectInputStream(byteInput);
                            response = (Response) objectInput.readObject();
                            return response;
                        }

                    } catch (SocketTimeoutException e) {
                        long now = System.currentTimeMillis();
                        if (now < deadline) {
                            // still within sliding deadline, continue waiting
                            continue;
                        }
                        break; // this attempt timed out, go to next attempt
                    } catch (Exception e) {
                        StdConsole.writeln("Error receiving response: " + e.getMessage());
                        break;
                    }
                }

                StdConsole.writeln("Attempt " + attempt + " of " + MAX_ATTEMPTS + " to reach server...");
                if (attempt >= MAX_ATTEMPTS) {
                    StdConsole.writeln("All attempts failed. Server is unavailable.");
                    break;
                }
                // else retry: loop will resend request
            }

            return response;

        } catch (IOException e) {
            StdConsole.writeln("Network error: " + e.toString());
            return new Response("Network error", Status.FAILED, "", null);
        }
    }
}