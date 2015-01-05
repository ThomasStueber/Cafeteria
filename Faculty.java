
public class Faculty {
    public static int faculty(int n, boolean recursive) {
        if (n < 0)
            return -1;
        if (n == 0)
            return 1;
        if (recursive)
            return calculateRecursive(n);
        else
            return calculateIterative(n);
    }

    private static int calculateRecursive(int n) {
        if (n == 1)
            return n;
        return n * calculateRecursive(n-1);
    }

    public static int calculateIterative(int n) {
        for (int i = n-1; i >= 1; i--)
            n *= i;
        return n;
    }
}
