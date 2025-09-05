import java.util.Scanner;

public class Lab2 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Enter your message:");
        String message = scanner.nextLine();

        int code_pow = (int) Math.ceil(Math.log(message.length()) / Math.log(2));
        int R, inf_bit, error_index = 0;
        boolean flag;
        String info = message;

        for (int r = 0; r < code_pow; r++) {
            flag = false;
            R = -1;
            for (int i = 1; i <= message.length(); i++) {
                if (Math.ceilMod(i, (int) Math.pow(2, r)) == 0) {
                    flag = !flag;
                }
                inf_bit = Integer.parseInt(String.valueOf(message.charAt(i - 1)));
                if (flag) {
                    if (R == -1) {
                        R = inf_bit;
                        info = info.substring(0, i - 1) + '-' + info.substring(i);
                    } else {
                        R = R ^ inf_bit;
                    }
                }
            }
            error_index += (int) Math.pow(2, r) * R;
        }

        if (error_index == 0) {
            System.out.println("No errors in message found");
        } else {
            switch (String.valueOf(info.charAt(error_index - 1))) {
                case "1":
                    info = info.substring(0, error_index - 1) + '0' + info.substring(error_index);
                    break;
                case "0":
                    info = info.substring(0, error_index - 1) + '1' + info.substring(error_index);
                    break;
                default:
                    break;
            }
            System.out.println("Error found on bit " + error_index);
        }
        System.out.println("Information of message: " + info.replaceAll("-", ""));
    }
}
