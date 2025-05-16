package lab8.client.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketTimeoutException;

import lab8.shared.io.console.StdConsole;
import lab8.shared.messages.Request;
import lab8.shared.messages.Response;
import lab8.shared.messages.Status;

public class NetworkClient {
    private static final String SERVER_IP = "192.168.10.80";//"77.234.196.4"; //"127.0.0.1"; // Пример
                                                           // IP сервера
    private static final int SERVER_PORT = 2224; // Пример порта сервера
    private static final int TIMEOUT = 5000; // Тайм-аут в миллисекундах (2 секунды)
    private static final int MAX_ATTEMPTS = 3; // Максимальное количество попыток
    // private StdConsole console = new StdConsole();

    public Response sendRequest(Request request) {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(TIMEOUT); // Устанавливаем тайм-аут

            // Сериализация Request в массив байтов
            ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
            ObjectOutputStream objectOutput = new ObjectOutputStream(byteOutput);
            objectOutput.writeObject(request);
            byte[] requestData = byteOutput.toByteArray();

            InetAddress serverAddress = InetAddress.getByName(SERVER_IP);
            DatagramPacket packet = new DatagramPacket(requestData, requestData.length, serverAddress, SERVER_PORT);

            boolean responseReceived = false;
            int attempts = 0;
            Response response = null;

            // Цикл попыток отправки и получения ответа
            while (!responseReceived && attempts < MAX_ATTEMPTS) {
                try {
                    socket.send(packet); // Отправка запроса

                    // Получение ответа
                    byte[] buffer = new byte[65535];
                    DatagramPacket responsePacket = new DatagramPacket(buffer, buffer.length);
                    socket.receive(responsePacket);

                    // Десериализация Response
                    ByteArrayInputStream byteInput = new ByteArrayInputStream(responsePacket.getData(), 0,
                            responsePacket.getLength());
                    ObjectInputStream objectInput = new ObjectInputStream(byteInput);
                    response = (Response) objectInput.readObject();

                    responseReceived = true;

                } catch (SocketTimeoutException e) {
                    attempts++;
                    StdConsole.writeln("Attempt " + attempts + " of " + MAX_ATTEMPTS + " to reach server...");
                    StdConsole.writeln("   Server did not respond within " + ((TIMEOUT / 1000) * attempts) + " seconds.");
                    if (attempts >= MAX_ATTEMPTS) {
                        StdConsole.writeln("All attempts failed. Server is unavailable.");
                        break;
                    }
                } catch (Exception e) {
                    StdConsole.writeln("Error sending request: " + e.getMessage());
                    break;
                }
            }

            return response;

        } catch (IOException e) {
            StdConsole.writeln("Network error: " + e.toString());
            return new Response("Network error", Status.FAILED, "", null);
        }
    }
}