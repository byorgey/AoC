import java.util.*;

public class Trampoline {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        ArrayList<Integer> jump = new ArrayList<>();
        while (in.hasNext()) {
            jump.add(in.nextInt());
        }

        int i = 0;
        int count = 0;
        while (i >= 0 && i < jump.size()) {
            count++;
            int j = jump.get(i);
            // jump.set(i, j+1);  // puzzle 1
            jump.set(i, j + (j >= 3 ? -1 : 1));
            i += j;
        }

        System.out.println(count);
    }
}
