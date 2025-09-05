public class testa {
    public static void main(String[] args) {
        // switch(args.length) {
        //     case 0: System.out.println("Hello, world!"); break;
        //     case 1: System.out.println("Hello, " + args[0] + "!");
        //     case 2: System.out.println("Hello, " + args[0] + " and " + args[1] + "!");
        //     default: System.out.println("Hello, all!");
        // }

        int a = 1;
        foo(a);
        System.out.print(a);
        // if (args.length == 0) {
        //     System.out.println("Hello, world!");
        // }
        // if (args.length == 1) {
        //     System.out.println("Hello, " + args[0] + "!");
        // }
        // if (args.length > 1) {
        //     System.out.println("Hello, " + args[0] + " and " + args[1] + "!");
        // }
        // if (args.length > 2) {
        //     System.out.println("Hello, all!");
        // }
    }

    public static void foo(int a){
        a+=10;
        // System.out.println(args);
    }
}
