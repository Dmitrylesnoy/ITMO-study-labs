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

import lab6.system.io.console.StdConsole;
import lab6.system.messages.Request;
import lab6.system.messages.Response;

public class NetworkClient {
    private static final String SERVER_IP = "127.0.0.1"; // Пример IP сервера
    private static final int SERVER_PORT = 5000; // Пример порта сервера
    private static final int TIMEOUT = 2000; // Тайм-аут в миллисекундах (2 секунды)
    private static final int MAX_ATTEMPTS = 3; // Максимальное количество попыток
    private StdConsole console=new StdConsole();

    public Response sendRequest(Request request) {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(TIMEOUT); // Устанавливаем тайм-аут

            // Сериализация Request в массив байтов
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(request);
            byte[] requestData = baos.toByteArray();

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
                    ByteArrayInputStream bais = new ByteArrayInputStream(responsePacket.getData(), 0,
                            responsePacket.getLength());
                    ObjectInputStream ois = new ObjectInputStream(bais);
                    response = (Response) ois.readObject();

                    responseReceived = true;

                } catch (SocketTimeoutException e) {
                    console.writeln("Server did not respond within " + (TIMEOUT / 1000) + " seconds.");
                    if (attempts == MAX_ATTEMPTS) {
                        console.writeln("All attempts failed. Server is unavailable.");
                    }
                }
                attempts++;
                console.writeln("Attempt " + attempts + " of " + MAX_ATTEMPTS + " to reach server...");
            }

            return response;

        } catch (IOException | ClassNotFoundException e) {
            console.writeln("Network error: " + e.toString());
            return null;
        }
    }
}