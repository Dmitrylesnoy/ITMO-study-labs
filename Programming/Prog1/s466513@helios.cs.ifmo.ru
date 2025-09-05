import java.util.ArrayList;
import java.util.List;
import java.util.Random;

// cd build/
// jar -cvfm sample.jar MANIFEST.MF -C classes/ .
// jar -c -f Lab1.jar -e Lab1 Lab1.class
// scp -P 2222 Lab1.java s466513@helios.cs.ifmo.ru:~/Prog1

public class Lab1 {

    private static List<Short> nums = new ArrayList();

    public static void main(String[] args) {
        short[] z = new short[] { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23 };
        float[] x = new float[10];
        double[][] z1 = new double[12][10];
        Random random = new Random();

        for (int i = 0; i < x.length; ++i) {
            x[i] = (float) (-15.0 + (double) random.nextFloat() * 23.0);
        }

        nums.add(Short.valueOf((short) 5));
        nums.add(Short.valueOf((short) 7));
        nums.add(Short.valueOf((short) 11));
        nums.add(Short.valueOf((short) 13));
        nums.add(Short.valueOf((short) 21));
        nums.add(Short.valueOf((short) 23));

        for (int i = 0; i < 12; ++i) {
            for (int j = 0; j < 10; ++j) {
                z1[i][j] = Calc(z[i], x[j]);
            }
        }

        printMatrix(z1);
    }

    static void printMatrix(double[][] matrix) {
        int lines_c = matrix.length;

        for (int i = 0; i < lines_c; ++i) {
            double[] line = matrix[i];

            for (int j = 0; j < line.length; ++j) {
                if (Double.isNaN(line[j])) {
                    System.out.print(line[j]);
                } else {
                    System.out.print((double) Math.round(line[j] * 100.0) / 100.0);
                }

                if (j + 1 < line.length) {
                    System.out.print(", ");
                }
            }

            System.out.println();
        }

    }

    static double Calc(short z, float x) {
        double ans;
        if (z == 3) {
            ans = Math.pow(3.0 * Math.tan((double) z), 2.0) / 2.0;
        } else if (nums.contains(z)) {
            ans = Math.log(Math.abs(Math.pow(Math.E, Math.PI / (double) x)));
        } else {
            ans = 3.0 / (Math.PI - Math.pow(
                    4.0 / (Math.pow(0.25 * (0.75 - Math.pow((double) x, 0.5 + (double) x)), 2.0) - 1.0), 3.0));
        }

        return ans;
    }
}
