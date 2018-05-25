import java.util.Arrays;
import java.util.Scanner;

public class Main {

    public static int upperBound(int[] array, int length, int value) {
        int low = 0;
        int high = length;
        while (low < high) {
            final int mid = (low + high) / 2;
            if (value >= array[mid]) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }

    public static void main(String args[]) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int k = sc.nextInt();
        int l = sc.nextInt();
        int arr[] = new int[n * k];
        for (int i = 0; i < n * k; i++) {
            arr[i] = sc.nextInt();
        }

        Arrays.sort(arr);
        int rg = upperBound(arr,n*k,arr[0] + l);
        if (rg <= n - 1) {
            System.out.println("0");

        } else {
            int iterator = 0;
            long ans = 0;
            for (int i = 0; i < n; i++) {
                ans += arr[iterator++];
                for (int j = 1; j < k; j++) {
                    if (rg - iterator > n - i + 1) {
                        iterator++;
                    } else
                        break;
                }
            }
            System.out.println(ans);
        }
    }
}