package nl.uva.sqrt;

public class RootCalculator {

    public static double calculateRoot(String number, String root) {
        double numberInt = Double.parseDouble(number);
        double rootInt = Double.parseDouble(root);
        if (rootInt <= 0) {
            throw new IllegalArgumentException("Root must be greater than 0");
        }
        return Math.pow(numberInt, 1.0 / rootInt);
    }
}
