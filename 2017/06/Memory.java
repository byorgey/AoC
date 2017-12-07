import java.util.*;

public class Memory {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        // Floyd's cycle-finding algorithm.

        int[] start = new int[16];
        int[] mem  = new int[16];
        int[] mem2 = new int[16];
        for (int i = 0; i < 16; i++) {
            start[i] = mem[i] = mem2[i] = in.nextInt();
        }

        // Start mem and mem2 at initial memory.  mem takes one step
        // at a time, mem2 takes two.  Invariant: mem = f^i(start),
        // mem2 = f^(2i)(start).

        int i = 1;
        step(mem);
        step(mem2); step(mem2);
        while (!eq(mem, mem2)) {
            step(mem);
            step(mem2); step(mem2);
            i++;
        }

        // Now we have the smallest i such that f^i(start) =
        // f^(2i)(start).  Reset mem2 to start, and start scanning
        // both mem and mem2 in parallel by one step, to find the
        // first repeated value, with index mu (because the period
        // must divide i, so the first repeated value will be repeated
        // i steps later).

        cp(start,mem2);

        int mu = 0;
        while (!eq(mem2,mem)) {
            step(mem2); step(mem); mu++;
        }

        System.out.println("mu = " + mu);

        // Now find the period lambda by scanning from mu step by step
        // until finding the first repeat.

        cp(mem2, mem);
        step(mem);
        int lambda = 1;
        while (!eq(mem2, mem)) {
            step(mem); lambda++;
        }

        System.out.println("lambda = " + lambda);

        System.out.println("First seen twice = mu + lambda = " + (mu + lambda));
    }

    public static void cp(int[] arr1, int[] arr2) {
        for (int i = 0; i < arr1.length; i++) {
            arr2[i] = arr1[i];
        }
    }

    public static boolean eq(int[] arr1, int[] arr2) {
        for (int i = 0; i < arr1.length; i++) {
            if (arr1[i] != arr2[i]) return false;
        }
        return true;
    }

    public static void step(int[] arr) {
        int max = arr[0];
        int maxix = 0;

        for (int i = 1; i < 16; i++) {
            if (arr[i] > max) { max = arr[i]; maxix = i; }
        }

        int blocks = arr[maxix];
        arr[maxix] = 0;
        for (int i = 1; i <= blocks; i++) {
            arr[(maxix+i) % 16]++;
        }
    }
}
