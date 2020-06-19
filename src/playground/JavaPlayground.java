package playground;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class JavaPlayground {
    public static void main(String[] args) {
        System.out.println("Hello Java!");
        /*
        Uncomment to see the solution for the challenge at: https://www.hackerrank.com/challenges/breaking-best-and-worst-records/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
        int[] input1 = {3, 4, 21, 36, 10, 28, 35, 5, 24, 42};
        int[] input2 = {10, 5, 20, 20, 4, 5, 2, 25, 1};
    
        int[] records1 = breakingRecords(input1);
        //int[] records2 = breakingRecords(input2);
    
        System.out.println("records1 = "+ records1[0] + "-"+records1[1]);
        //System.out.println("records2 = "+ records2[0] + "-"+records2[1]);*/
    }
    
    static int[] breakingRecords(int[] scores) {
        final int maxPos = 0;
        final int minPos = 1;
        int minScore = scores[0];
        int maxScore = scores[0];
        int[] answers = new int[2];
        Map<String, Integer> counts = new HashMap<String, Integer>();
        counts.put("max", 0);
        counts.put("min", 0);
        
        for (int index = 0; index < scores.length; index++) {
            if (index == 0 && isMax(scores, maxScore)) {
                answers[maxPos] = 0;
            } else if (index ==0 && isMin(scores, minScore)) {
                answers[minPos] = 0;
            } else if (scores[index] > maxScore) {
                maxScore = scores[index];
                counts.put("max", counts.get("max") + 1);
                answers[maxPos] = counts.get("max");
            } else if (scores[index] < minScore) {
                minScore = scores[index];
                counts.put("min", counts.get("min") + 1);
                answers[minPos] = counts.get("min");
            }
            
        }
        return answers;
    }
    
    private static boolean isMax(int[] scores, int value) {
        return !Arrays.stream(scores).filter(score -> score > value).findAny().isPresent();
    }
    
    private static boolean isMin(int[] scores, int value) {
        return !Arrays.stream(scores).filter(score -> score < value).findAny().isPresent();
    }
}
