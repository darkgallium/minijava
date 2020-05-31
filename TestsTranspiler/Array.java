class Array {
    public static void main(String[] args) {
      Main obj;
      obj = new Main();
      System.out.println(obj.init());
    }
}

class Main {
    public int init() {
	     return this.createArray(42)[3];
    }

    public int[] createArray(int n) {
        int[] a;
        int i;

        a = new int[n];
        i = 0;
        while (i < n) {
            a[i] = i;
            i = i + 1;
            System.out.println(i);
        }
        return a;
    }
}
