package lab.programming.textCreator;

// interface Innertest1 {
//     void say1();
// }

// interface Innertest2 extends Innertest1 {
//     void say2();
// }

public class Test {

    public static void main(String[] args) {
        interface Inter1{
            void Say();

            static void Hello() {
                System.out.println("Hello i am ststaic interface");
            }
        }
        interface Inter2 {
            void Say();
        }

        class Abob2 {
            Abob2() {
            }

            int getNum(int a) {
                return a;
            }

            float getNum(float b) {
                return b;
            }
        }

        class Abob implements Inter1{
            private int i;

            Abob() {
            }
            Abob(int i) {
                this.i = i;
            }
            public void Say() {
                System.out.println("testtest");
            }

            // @Override
            // static void Hello() {
            //     System.out.println("Hello im a new class");
            // }
        }

        Abob obj1 = new Abob();
        Inter1 obj2 = obj1;
        Abob2 obj3 = new Abob2();
        Object obj4 = new Object();
        
        obj1.Hello();
        Inter1.Hello();
        // obj2.Hello();

        System.out.println(obj3.getNum(22));
        System.out.println(obj3.getNum(2.2));

        // System.out.println(obj1.hashCode());
        // System.out.println(obj2.hashCode());
        // System.out.println(obj1.equals(obj2));

        // System.out.println(obj2.toString());


        System.out.println("Hello world!");
    }
}
