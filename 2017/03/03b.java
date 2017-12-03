public class Spiral {
    static int[][] arr;

    public static void main(String[] args) {

        arr = new int[20][20];

        int x = 10, y = 10;

        arr[x][y] = 1;
        int val = 1;
        int ring = 0;
        while (true) {
            x += 1;
            y -= 1;
            ring += 2;

            for (int i = 0; i < ring; i++) {
                y -= 1;
                fill(x,y);
            }
            for (int i = 0; i < ring; i++) {
                x -= 1;
                fill(x,y);
            }
            for (int i = 0; i < ring; i++) {
                y += 1;
                fill(x,y);
            }
            for (int i = 0; i < ring; i++) {
                x += 1;
                fill(x,y);
            }
        }
    }

    public static void fill(int x, int y) {
        int sum = 0;
        for (int i = -1; i <= 1; i++) {
            for (int j = -1; j <= 1; j++) {
                sum += arr[x+i][y+j];
            }
        }
        arr[x][y] = sum;
        if (sum > 368078) {
            System.out.println(sum);
            System.exit(0);
        }
    }
}
