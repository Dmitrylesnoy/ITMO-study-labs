package lab8.client.utils;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import javafx.application.Platform;
import lab8.shared.builders.SpaceMarineBuilder;
import lab8.shared.commands.Add;
import lab8.shared.commands.AddRandom;
import lab8.shared.commands.Clear;
import lab8.shared.commands.Command;
import lab8.shared.commands.ExecuteScript;
import lab8.shared.commands.Exit;
import lab8.shared.commands.FilterStartsWithAchievements;
import lab8.shared.commands.Help;
import lab8.shared.commands.Info;
import lab8.shared.commands.Load;
import lab8.shared.commands.LoadPart;
import lab8.shared.commands.Login;
import lab8.shared.commands.MinByMeleeWeapon;
import lab8.shared.commands.PrintUniqueLoyal;
import lab8.shared.commands.RemoveByID;
import lab8.shared.commands.RemoveGreater;
import lab8.shared.commands.RemoveLower;
import lab8.shared.commands.EditItem;
import lab8.shared.commands.Show;
import lab8.shared.commands.Sort;
import lab8.shared.commands.UpdateId;
import lab8.shared.io.console.ClientConsole;
import lab8.shared.io.console.StdConsole;
import lab8.shared.messages.Request;
import lab8.shared.messages.Response;
import lab8.shared.messages.Status;
import lab8.shared.model.SpaceMarine;

/**
 * The Handler class is responsible for processing user input commands.
 * It utilizes a Router to execute commands based on requests created from user
 * input.
 * This class provides methods for running commands, making requests, and
 * managing the console interface.
 */
public class Handler {
    // private StdConsole console;
    Map<String, Command> cmds = new HashMap<>();
    private static NetworkClient network = new NetworkClient();
    private static String username;
    private static String password;
    private ClientConsole console = ClientConsole.getInstance();
    private static Handler instance;
    private static Stack<SpaceMarine> localMarines = new Stack<SpaceMarine>();
    private static boolean login = false;

    /**
     * Default constructor for the Handler class.
     */
    public Handler() {
        cmds.put("add", new Add());
        cmds.put("add_random", new AddRandom());
        cmds.put("clear", new Clear());
        cmds.put("exit", new Exit());
        cmds.put("load", new Load());
        // cmds.put("save", new Save());
        cmds.put("info", new Info());
        cmds.put("show", new Show());
        cmds.put("sort", new Sort());
        cmds.put("filter_starts_with_achievements", new FilterStartsWithAchievements());
        cmds.put("min_by_meleeweapon", new MinByMeleeWeapon());
        cmds.put("remove_by_id", new RemoveByID());
        cmds.put("remove_greater", new RemoveGreater());
        cmds.put("remove_lower", new RemoveLower());
        cmds.put("update_id", new UpdateId());
        cmds.put("print_unique_loyal", new PrintUniqueLoyal());
        cmds.put("execute_script", new ExecuteScript());
        cmds.put("help", new Help(cmds));
    }

    public void run(Request request) {
        try {
            if (request.username()==null || request.password()==null)
                request = new Request(request.command(),request.args(), username, hashPassword(password));
            Response response = network.sendRequest(request);
            console.writeln(response.toString());
            if (response.status() == Status.CLOSE) {
                request.command().execute();
                Platform.exit();
                System.exit(0);
            }
            refreshCollection();
        } catch (NullPointerException e) {
            console.writeln(e.toString());

        } catch (Exception e) {
            console.writeln(e.toString());
        }
    }

    /**
     * Runs the command based on user input read from the console.
     */
    public void run() {
        // Request request = makeRequest(console.read());
        try {
            Request request = makeRequest(console.read());
            if (request.command().equals(Exit.class)) {
                Platform.exit();
                System.exit(0);
            }
            if (request.command().equals(Show.class)) {
                refreshCollection();
                console.writeln(localMarines.toString());
                return;
            }
            run(request);
        } catch (NullPointerException e) {
            console.writeln(e.toString());

        } catch (Exception e) {
            console.writeln(e.toString());
        }

    }

    /**
     * Creates a Request object from the user input string.
     *
     * @param input the user input string
     * @return the created Request object
     * @throws IOException
     */
    public Request makeRequest(String input) throws IOException {
        String[] inp_split = null;
        String[] inp_args = null;
        try {
            inp_split = input.strip().split("\s+");
        } catch (Exception e) {
            inp_split = new String[] {};
        }

        if (inp_split.length > 0 && inp_split[0].strip() != "") {
            if (inp_split.length > 1) {
                inp_args = Arrays.copyOfRange(inp_split, 1, inp_split.length);
            }
        }

        String name = inp_split[0].toLowerCase();
        Command cmd;
        Object cmdArgs = null;

        if (cmds.containsKey(name)) {
            cmd = cmds.get(name);

            if (cmd.getClass().equals(Add.class) ||
                    cmd.getClass().equals(RemoveGreater.class) ||
                    cmd.getClass().equals(RemoveLower.class))
                cmdArgs = new SpaceMarineBuilder().build();
            if (cmd.getClass().equals(AddRandom.class))
                cmdArgs = inp_args != null ? Integer.parseInt(inp_args[0]) : 1;
            if (cmd.getClass().equals(FilterStartsWithAchievements.class))
                cmdArgs = inp_args != null ? inp_args[0] : null;
            if (cmd.getClass().equals(RemoveByID.class))
                cmdArgs = inp_args != null ? Long.parseLong(inp_args[0]) : null;
            if (cmd.getClass().equals(UpdateId.class))
                cmdArgs = new SpaceMarineBuilder().setId(inp_args != null ? Long.parseLong(inp_args[0]) : -1).build();
            if (cmd.getClass().equals(ExecuteScript.class)) {
                cmdArgs = inp_args != null ? inp_args[0] : null;
                cmd.setArgs(cmdArgs).execute();
            }
        } else
            throw new UnsupportedOperationException("Unknown command");

        return new Request(cmd, cmdArgs, username, hashPassword(password));
    }

    private static String hashPassword(String password) {
        try {
            if (password == null) {
                return null;
            }

            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] hashBytes = md.digest(password.getBytes());

            StringBuilder sb = new StringBuilder();
            for (byte b : hashBytes) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
    }

    public static void setUser(String username, String password) {
        Handler.username = username;
        Handler.password = password;
    }

    public Stack<SpaceMarine> getCollection() {
        refreshCollection();
        return localMarines;
    }

    public void updateItem(SpaceMarine edited) {
        run(new Request(new EditItem(), edited, username, hashPassword(password)));
        refreshCollection();
    }

    public static boolean tryLogin(String usernameT, String passwordT) {
        Response response = network.sendRequest(new Request(new Login(), null, usernameT, hashPassword(passwordT)));
        StdConsole.writeln("answer recived");
        if (response.status().equals(Status.COMPLETE)) {
            setUser(usernameT, passwordT);
            StdConsole.writeln("Login succes");
            login = true;
        } else
            login = false;
        return login;
    }

    public static Handler getInstance() {
        return instance == null ? instance = new Handler() : instance;
    }

    public static void refreshCollection() {
        if (!login)
            return;
        Request request = null;
        Response response = null;
        Deque accumulate = new ArrayDeque<SpaceMarine>(0);

        int part = 0;
        do {
            part = Integer.parseInt(response != null && response.output() != null ? response.output() : "-1") + 1;
            request = new Request(new LoadPart(), part, username, hashPassword(password));
            response = network.sendRequest(request);
            if (response.listMarine() != null)
                accumulate.addAll(response.listMarine());
        } while (response != null && response.status() != Status.COMPLETE);
        localMarines.clear();
        localMarines.addAll(accumulate);
    }
}
